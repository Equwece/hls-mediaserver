{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Users.Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (pack)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (toField))
import GHC.Generics (Generic)

data User = User
  { id :: UUID,
    username :: String,
    password :: String,
    auth_type :: AuthType,
    create_date :: UTCTime
  }
  deriving (Generic, Show, Eq)

instance FromJSON User

instance ToJSON User

instance ToRow User

instance FromRow User

instance Data.OpenApi.ToSchema User

data AuthType = Regular | Admin deriving (Generic, Show, Eq)

instance FromJSON AuthType

instance ToJSON AuthType

instance ToField AuthType where
  toField Regular = Escape . pack $ "Regular"
  toField Admin = Escape . pack $ "Admin"

instance FromField AuthType where
  fromField f dat = case dat of
    Just "Regular" -> pure Regular
    Just "Admin" -> pure Admin
    _ -> error "Wrong auth type"

instance Data.OpenApi.ToSchema AuthType
