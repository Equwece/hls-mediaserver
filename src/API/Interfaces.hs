{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module API.Interfaces (AppEnvironment (..), Logger (..)) where

import API.External.Postgres (PostgresClass)
import Data.Text (Text)

data AppEnvironment where
  AppEnvironment ::
    (PostgresClass b) =>
    { logger :: Logger,
      db :: b,
      jwtEncodeSecret :: Text
    } ->
    AppEnvironment

newtype Logger = Logger {logMsg :: String -> IO ()}
