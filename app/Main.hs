{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import API.APISpec (proxyAPI)
import API.External.Postgres
import API.Handlers (apiServer)
import API.Interfaces (AppEnvironment (AppEnvironment, db, logger), Logger (Logger, logMsg))
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad ((>=>))
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Network.Wai.Handler.Warp (run)
import Servant (Application, serve)
import System.Environment (getEnv)
import System.Log.FastLogger (LogStr, LogType' (LogStdout), ToLogStr (toLogStr), defaultBufSize, withFastLogger)

hlsApp :: AppEnvironment -> Application
hlsApp appEnv = serve proxyAPI (apiServer appEnv)

main :: IO ()
main = do
  withFastLogger (LogStdout defaultBufSize) $ \fastLogger -> do
    _ <- loadFile defaultConfig
    dbConnStr <- getEnv "POSTGRES_CONN_STR"
    pgConn <- connectPostgreSQL . T.encodeUtf8 . T.pack $ dbConnStr
    let logger = Logger {logMsg = wrapLogMsg >=> fastLogger}
        db = PostgresDB pgConn
        appEnv = AppEnvironment {..}
    logMsg logger "API has started..."
    run 8080 (hlsApp appEnv)

wrapLogMsg :: String -> IO LogStr
wrapLogMsg msg = do
  currentTime <- getCurrentTime
  return . toLogStr $ show currentTime <> " " <> msg <> "\n"
