{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Users.Handlers where

import API.External.Postgres (PostgresClass (addUserQuery, deleteUserByIdQuery, getUserByIdQuery, listUsersQuery, updateUserByIdQuery))
import API.Interfaces
  ( AppEnvironment (AppEnvironment, db, logger),
    Logger (logMsg),
  )
import API.Users.Models
import Control.Monad.Cont (MonadIO (liftIO))
import Data.UUID (UUID)
import Servant (Handler, NoContent (NoContent), err404, throwError, type (:<|>) ((:<|>)))

userServer appEnv@(AppEnvironment {..}) = listUsers :<|> addUser :<|> (userEntityServer appEnv)
  where
    listUsers :: Handler [User]
    listUsers = do
      users <- liftIO $ listUsersQuery db
      liftIO $ logMsg logger "List Users"
      return users
    addUser :: User -> Handler UUID
    addUser newUser = do
      newUserId <- head <$> liftIO (addUserQuery db newUser)
      liftIO $ logMsg logger ("Add User " <> show newUserId)
      return newUserId

userEntityServer (AppEnvironment {..}) uId = getResource uId :<|> deleteUser uId :<|> updateUser uId
  where
    getResource :: UUID -> Handler User
    getResource uId = do
      u <- liftIO $ getUserByIdQuery db uId
      liftIO $ logMsg logger ("Get User " <> show uId)
      if not (null u)
        then return (head u)
        else throwError err404

    deleteUser :: UUID -> Handler NoContent
    deleteUser uId = do
      rowCount <- liftIO $ deleteUserByIdQuery db uId
      liftIO $ logMsg logger ("Delete User " <> show uId)
      if rowCount /= 0
        then return NoContent
        else throwError err404

    updateUser :: UUID -> User -> Handler User
    updateUser uId newUser = do
      rowCount <- liftIO $ updateUserByIdQuery db newUser uId
      liftIO $ logMsg logger ("Update User " <> show uId)
      if rowCount /= 0
        then do
          u <- liftIO $ getUserByIdQuery db uId
          if not (null u)
            then return (head u)
            else throwError err404
        else throwError err404
