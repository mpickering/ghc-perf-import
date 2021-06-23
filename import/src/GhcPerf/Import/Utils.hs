{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module GhcPerf.Import.Utils where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.Map.Strict as M
import Data.Int
import qualified Data.Text as T

import GhcPerf.Import.Types

defaultConnInfo = defaultConnectInfo { connectDatabase = "ghc_perf", connectUser = "ben", connectPassword = "mudpie" }

ensureTestEnvExists :: Connection -> TestEnvName -> IO ()
ensureTestEnvExists conn testEnv = do
    execute conn
        [sql| INSERT INTO test_envs (test_env_name)
              VALUES (?)
              ON CONFLICT DO NOTHING
            |]
        (Only testEnv)
    return ()

addMetrics :: Connection
           -> Commit -> TestEnvName -> M.Map MetricName Double -> IO Int64
addMetrics conn commit testEnv tests = withTransaction conn $ do
    putStrLn "Inserting tests"
    executeMany conn
        [sql| INSERT INTO tests (test_name)
              VALUES (?)
              ON CONFLICT (test_name) DO NOTHING|]
        (map Only $ M.keys tests)

    putStrLn "Selecting tests"
    testIds <- M.fromList <$> query conn
        [sql| SELECT test_name, test_id
              FROM tests
              WHERE test_name IN ? |]
        (Only $ In $ M.keys tests)

    putStrLn "Inserting commits"
    execute conn
        [sql| INSERT INTO commits (commit_sha)
              VALUES (?)
              ON CONFLICT DO NOTHING |]
        (Only commit)

    putStrLn "Selecting commits"
    [Only commitId] <- query conn
        [sql| SELECT commit_id
              FROM commits
              WHERE commit_sha = ? |]
        (Only commit)

    putStrLn "Inserting environments"
    execute conn
        [sql| INSERT INTO test_envs (test_env_name)
              VALUES (?)
              ON CONFLICT (test_env_name) DO NOTHING |]
        (Only testEnv)

    putStrLn "Selecting environments"
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
              VALUES (?,?,?,?)
              ON CONFLICT DO NOTHING |]
        results


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
