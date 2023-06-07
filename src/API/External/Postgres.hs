{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.External.Postgres where

import qualified API.Models as AM
import API.Resource.Models (Resource (..))
import API.Users.Models (User (User, auth_type, password, username))
import Data.Int (Int64)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, FromRow, execute, query, query_)
import Database.PostgreSQL.Simple.FromField (FromField)

newtype PostgresDB = PostgresDB {dbConnection :: Connection}

class PostgresClass a where
  listResourcesQuery :: a -> IO [Resource]
  getResourceByIdQuery :: a -> UUID -> IO [Resource]
  getResourceByHash :: a -> String -> IO [Resource]
  addResource :: a -> Resource -> IO [UUID]

  listUsersQuery :: a -> IO [User]
  addUserQuery :: a -> User -> IO [UUID]
  getUserByIdQuery :: a -> UUID -> IO [User]
  getUserByUsernameQuery :: a -> String -> IO [User]
  deleteUserByIdQuery :: a -> UUID -> IO Int64
  updateUserByIdQuery :: a -> User -> UUID -> IO Int64

  addRefreshQuery :: a -> UUID -> IO [UUID]
  deleteRefreshByIdQuery :: a -> UUID -> IO Int64
  deleteRefreshByUserIdQuery :: a -> UUID -> IO Int64
  getRefreshByIdQuery :: a -> UUID -> IO [(UUID, UUID)]

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

  listUsersQuery (PostgresDB pgConn) = do
    query_ pgConn "SELECT id,username,password,auth_type,create_date FROM public.user"

  addUserQuery (PostgresDB pgConn) (User {..}) = do
    query
      pgConn
      "INSERT INTO public.user (username, password, auth_type) VALUES (?, ?, ?) RETURNING id"
      (username, password, auth_type)

  getUserByIdQuery (PostgresDB pgConn) uId = do
    query
      pgConn
      "SELECT id,username,password,auth_type,create_date FROM public.user WHERE id=?"
      [uId]

  deleteUserByIdQuery (PostgresDB pgConn) uId = do
    execute
      pgConn
      "DELETE FROM public.user WHERE id=?"
      [uId]

  updateUserByIdQuery (PostgresDB pgConn) (User {..}) uId = do
    execute
      pgConn
      "UPDATE public.user SET username=?,password=?,auth_type=? WHERE id=?"
      (username, password, auth_type, uId)

  getUserByUsernameQuery (PostgresDB pgConn) uName = do
    query
      pgConn
      "SELECT id,username,password,auth_type,create_date FROM public.user WHERE username=?"
      [uName]

  addRefreshQuery (PostgresDB pgConn) uId = do
    query
      pgConn
      "INSERT INTO public.refresh (user_id) VALUES (?) RETURNING id"
      [uId]

  deleteRefreshByIdQuery (PostgresDB pgConn) refreshId = do
    execute
      pgConn
      "DELETE FROM public.refresh WHERE id=?"
      [refreshId]

  deleteRefreshByUserIdQuery (PostgresDB pgConn) uId = do
    execute
      pgConn
      "DELETE FROM public.refresh WHERE user_id=?"
      [uId]

  getRefreshByIdQuery (PostgresDB pgConn) rId = do
    query
      pgConn
      "SELECT id,user_id FROM public.refresh WHERE id=?"
      [rId]
