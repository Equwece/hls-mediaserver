{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.External.Postgres where

import API.Resource.Models (Resource)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, query, query_)

newtype PostgresDB = PostgresDB {dbConnection :: Connection}

class PostgresClass a where
  listResourcesQuery :: a -> IO [Resource]
  getResourceByIdQuery :: a -> UUID -> IO [Resource]

instance PostgresClass PostgresDB where
  listResourcesQuery (PostgresDB pgConn) = do
    query_ pgConn "SELECT id,title,type,segmented FROM resource" :: IO [Resource]

  getResourceByIdQuery (PostgresDB pgConn) resId = do
    query pgConn "SELECT id,title,type,segmented FROM resource WHERE id=?" [resId] :: IO [Resource]
