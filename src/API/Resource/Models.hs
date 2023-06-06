{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Resource.Models (Resource (..), ResourceType (..)) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.OpenApi
  ( HasProperties (properties),
    HasRequired (required),
    HasType (type_),
    NamedSchema (NamedSchema),
    OpenApiType (OpenApiObject),
    ToSchema (..),
    declareSchemaRef,
  )
import Data.Proxy (Proxy (..))
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (toField))
import GHC.Generics (Generic)

-- Media resource data type
data Resource = Resource
  { resourceId :: UUID,
    resourceTitle :: String,
    resourceType :: ResourceType,
    isSegmented :: Bool,
    resourceHash :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON Resource

instance ToJSON Resource

instance ToRow Resource

instance FromRow Resource

instance ToSchema Resource

-- Media resource type
data ResourceType = Audio | Video deriving (Generic, Show, Eq)

instance ToSchema ResourceType where
  declareNamedSchema _ = do
    resourceTypeSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $
      NamedSchema (Just "ResourceType") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("audio", resourceTypeSchema),
                 ("video", resourceTypeSchema)
               ]
          & required .~ ["audio", "video"]

instance FromJSON ResourceType where
  parseJSON (String "audio") = return Audio
  parseJSON (String "video") = return Video
  parseJSON _ = error "Resource type must be one of audio/video"

instance ToJSON ResourceType where
  toJSON Audio = String "audio"
  toJSON Video = String "video"

instance ToField ResourceType where
  toField Audio = Escape . pack $ "audio"
  toField Video = Escape . pack $ "video"

instance FromField ResourceType where
  fromField f dat = case dat of
    Just "audio" -> pure Audio
    Just "video" -> pure Video
    _ -> error "Wrong resource type"

instance FromRow UUID where
  fromRow = field
