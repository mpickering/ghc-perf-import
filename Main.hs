{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Int
import Control.Monad (void)
import Data.DList (DList, singleton)
import Data.Foldable
import Options.Applicative
import System.FilePath
import Control.Exception

import Types
import Slurp
import SummarizeResults

ingest :: Connection
       -> Commit -> TestEnvName -> M.Map TestName Double -> IO Int64
ingest conn commit testEnv tests = withTransaction conn $ do
    executeMany conn
        [sql| INSERT INTO tests (test_name)
              VALUES (?)
              ON CONFLICT (test_name) DO NOTHING|]
        (map Only $ M.keys tests)

    testIds <- M.fromList <$> query conn
        [sql| SELECT test_name, test_id
              FROM tests
              WHERE test_name IN ? |]
        (Only $ In $ M.keys tests)

    execute conn
        [sql| INSERT INTO commits (commit_sha)
              VALUES (?)
              ON CONFLICT DO NOTHING |]
        (Only commit)

    [Only commitId] <- query conn
        [sql| SELECT commit_id
              FROM commits
              WHERE commit_sha = ? |]
        (Only commit)

    [Only testEnvId] <- query conn
        [sql| SELECT test_env_id
              FROM test_envs
              WHERE test_env_name = ? |]
        (Only testEnv)

    let results :: [(Int, Int, Int, Double)]
        results = [ (commitId, testEnvId, testId, realToFrac value)
                  | (testName, value) <- M.toList tests
                  , Just testId <- pure $ M.lookup testName testIds
                  ]
    executeMany conn
        [sql| INSERT INTO results (commit_id, test_env_id, test_id, result_value)
              VALUES (?,?,?,?) |]
        results

connInfo = defaultConnectInfo { connectDatabase = "ghc_perf", connectUser = "ben", connectPassword = "mudpie" }


args :: Parser (TestEnvName, [FilePath])
args =
    (,)
      <$> option str (short 'e' <> long "env" <> help "test environment name" <> metavar "ENV")
      <*> some (argument str $ help "log files" <> metavar "FILE")

getFileCommit :: FilePath -> Commit
getFileCommit = dropExtensions . takeFileName

findMissingCommits :: Connection -> TestEnvName -> [Commit] -> IO (S.Set Commit)
findMissingCommits conn testEnv commits =
    S.fromList <$> query conn
        [sql| SELECT x.column1
              FROM ? as x
              WHERE x.column1 NOT IN (
                  SELECT commit_sha
                  FROM results_view
                  WHERE test_env = ?
              )
            |]
        (Values ["text"] (map Only commits), testEnv)


main :: IO ()
main = do
    (testEnv, files) <- execParser $ info (helper <*> args) mempty
    let printExc :: String -> SomeException -> IO ()
        printExc fname exc = putStrLn $ fname++": "++show exc

    conn <- connect connInfo
    missing <- findMissingCommits conn testEnv (map getFileCommit files)
    let toImport = filter (\fname -> getFileCommit fname `S.member` missing) files

    forM_ toImport $ \fname -> handle (printExc fname) $ do
        let commit = getFileCommit fname
        results <- M.fromList <$> parseResults fname
        putStrLn $ commit++": "++show (M.size results)++" results"
        void $ ingest conn commit testEnv results

    close conn
