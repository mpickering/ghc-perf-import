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

        parsed :: Measurements
        parsed = parseLog (pkgName, version) contents
    print (pkgName, version, parts, logPath, baseName)
    -- Do not force "parsed" here.. some logs take a long time to parse so
    -- if you end up skipping the import it takes much longer.
    void $ addMeasurements conn (job_id <> "/" <> T.pack baseName) job_name commit parsed

-- | Check to see whether we have already inserted this job, and if not,
-- run the continuation with the id
withProvenanceId :: Connection -> T.Text -> (Int64 -> IO Int64) -> IO Int64
withProvenanceId conn job_id k = do
    (prov_id :: [Only Int64]) <- query conn
        [sql| SELECT provenance_id
              FROM provenance
              WHERE name = ?
              LIMIT 1 |]
        (Only $ job_id)
    case prov_id of
      -- Already inserted this job
      [Only p_id] -> print ("Skipping:" <> job_id) >> return 0
      [] ->  do
        execute conn
          [sql| INSERT INTO provenance (name)
                VALUES (?)
          |]
          (Only job_id)
        [Only prov_id] <- query conn
          [sql| SELECT provenance_id
                FROM provenance
                WHERE name = ?
                LIMIT 1 |]
          (Only $ job_id)
        k prov_id



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

