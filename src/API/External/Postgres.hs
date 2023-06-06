{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.External.Postgres where

import API.Resource.Models (Resource (..))
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, query, query_)

newtype PostgresDB = PostgresDB {dbConnection :: Connection}

class PostgresClass a where
  listResourcesQuery :: a -> IO [Resource]
  getResourceByIdQuery :: a -> UUID -> IO [Resource]
  getResourceByHash :: a -> String -> IO [Resource]
  addResource :: a -> Resource -> IO [UUID]

instance PostgresClass PostgresDB where
  listResourcesQuery (PostgresDB pgConn) = do
    query_ pgConn "SELECT id,title,type,segmented,hash FROM resource" :: IO [Resource]

  getResourceByIdQuery (PostgresDB pgConn) resId = do
    query pgConn "SELECT id,title,type,segmented,hash FROM resource WHERE id=?" [resId] :: IO [Resource]

  getResourceByHash (PostgresDB pgConn) hash = do
    query
      pgConn
      "SELECT id,title,type,segmented,hash FROM resource WHERE hash=?"
      [hash] ::
      IO [Resource]

  addResource (PostgresDB pgConn) (Resource {..}) = do
    query
      pgConn
      "INSERT INTO resource (title, type, segmented, hash) VALUES (?, ?, ?, ?) RETURNING id"
      (resourceTitle, resourceType, isSegmented, resourceHash)
