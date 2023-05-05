{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module API.Interfaces (AppEnvironment (..), Logger (..)) where

import API.External.Postgres (PostgresClass)

data AppEnvironment where
  AppEnvironment ::
    (PostgresClass b) =>
    {logger :: Logger, db :: b} ->
    AppEnvironment

newtype Logger = Logger {logMsg :: String -> IO ()}
