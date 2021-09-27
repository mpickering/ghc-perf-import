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
import Data.List.Split
import Data.List (intercalate)

testEnv :: TestEnvName
testEnv = TestEnvName "head-hackage"

args :: Parser (String, T.Text, T.Text, Commit, [FilePath])
args =
    (,,,,)
      <$> option str (short 'c' <> long "conn-string" <> help "PostgreSQL connection string")
      <*> option (T.pack <$> str) (short 'i' <> long "job-id" <> help "Job Id")
      <*> option (T.pack <$> str) (short 'j' <> long "job-name" <> help "Job Name")
      <*> option (Commit <$> str) (short 'C' <> long "commit" <> help "which commit these logs were computed from")
      <*> some (argument str (help "log files"))


importLog :: Connection -> T.Text -> T.Text ->  Commit -> FilePath -> IO ()
importLog conn job_id job_name commit logPath = do
    contents <- TIO.readFile logPath
    let pkgName = PackageName $ T.pack $ pkg
        baseName = takeBaseName logPath
        parts = splitOn "-" baseName
        pkg = intercalate "-" (init parts)
        version = Version $ T.pack $ last parts

        parsed :: Maybe Measurements
        parsed = parseLog (pkgName, version) contents
    case parsed of
      Nothing -> print ("Failed", pkgName, version)
      Just m | isEmptyMeasurements m -> print ("No Measurements", pkgName, version)
      Just m -> do
        print ("Importing", pkgName, version, parts, logPath, baseName)
        void $ addMeasurements conn (job_id <> "/" <> T.pack baseName) job_name commit m


addMeasurements :: Connection
           -> T.Text
           -> T.Text
           -> Commit
           -> Measurements
           -> IO Int64
addMeasurements conn job_id job_name commit measures = withTransaction conn $ withProvenanceId conn job_id $ \pid ->  do

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

    let results :: [(Int64, Int, T.Text, T.Text, T.Text, T.Text, T.Text,
                        Integer, Double)]
        results = [ (pid, commitId, job_name, pkgName, version, modName, passName, alloc, time)
                  | let Measurements ms = measures
                  , ((PackageName pkgName, Version version, ModuleName modName, PassName passName), Metrics alloc time) <- M.toList ms
                  ]

    putStrLn ("Inserting: " ++ show (length results))
    executeMany conn
        [sql| INSERT INTO head_hackage_results (provenance_id, commit_id, job_name, package, version, module, compiler_pass, allocs, time)
              VALUES (?,?,?,?,?,?,?,?,?)
        |]
        results

main :: IO ()
main = do
    (connString, job_id, job_name, commit, logFiles) <- execParser $ info (helper <*> args) mempty
    conn <- connectPostgreSQL $ BS.pack connString
    mapM_ (importLog conn job_id job_name commit) logFiles

