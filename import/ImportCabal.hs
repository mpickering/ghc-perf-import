{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

import GhcPerf.Import.HeadHackage
import GhcPerf.Import.Utils
import GhcPerf.Import.Types
import Data.Int
import Data.Char
import Data.List (intercalate)

testEnv :: TestEnvName
testEnv = TestEnvName "cabal-test"

args :: Parser (String, T.Text, [FilePath])
args =
    (,,)
      <$> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> option (T.pack <$> str) (short 'i' <> long "job-id" <> help "Job Id")
      <*> some (argument str (help "log dirs"))

strip_newline = reverse . dropWhile isSpace . reverse

importLog :: Connection -> T.Text -> FilePath -> IO ()
importLog conn job_id logDir = do
    ghc_commit <- Commit . strip_newline <$> readFile (logDir </> "ghc_commit")
    cabal_commit <- T.strip <$> TIO.readFile (logDir </> "cabal_commit")
    forM_ [0,1,2] $ \opt_level -> do
      contents <- TIO.readFile (logDir </> ("Cabal-O" <> show opt_level) </> "log")
      let pkgName = PackageName $ "Cabal"
          version = Version $ cabal_commit

          parsed :: Measurements
          parsed = parseLog (pkgName, version) contents
      print (pkgName, version)
      if isEmptyMeasurements parsed
        then return ()
        else void $ addMeasurements conn (job_id <> "/" <> T.pack (show opt_level)) ghc_commit opt_level parsed



addMeasurements :: Connection
           -> T.Text
           -> Commit
           -> Int
           -> Measurements
           -> IO Int64
addMeasurements conn job_id commit opt_level measures = withTransaction conn $ withProvenanceId conn job_id $ \pid ->  do

    execute conn
        [sql| INSERT INTO commits (commit_sha)
              VALUES (?)
              ON CONFLICT DO NOTHING |]
        (Only commit)
    -- The commit should be a GHC commit.. which should already be in the
    -- database, if not, that's bad
    [Only commitId] <- query conn
        [sql| SELECT commit_id
              FROM commits
              WHERE commit_sha = ? |]
        (Only commit)

    let results :: [(Int64, Int, T.Text, Int, T.Text, T.Text,
                        Integer, Double)]
        results = [ (pid, commitId, version, opt_level, modName, passName, alloc, time)
                  | let Measurements ms = measures
                  , ((PackageName pkgName, Version version, ModuleName modName, PassName passName), Metrics alloc time) <- M.toList ms
                  ]

    putStrLn ("Inserting: " ++ show (length results))
    executeMany conn
        [sql| INSERT INTO cabal_results (provenance_id, commit_id, version, opt_level, module, compiler_pass, allocs, time)
              VALUES (?,?,?,?,?,?,?,?)
        |]
        results

main :: IO ()
main = do
    (connString, job_id, logDirs) <- execParser $ info (helper <*> args) mempty
    conn <- connectPostgreSQL $ BS.pack connString
    mapM_ (importLog conn job_id) logDirs

