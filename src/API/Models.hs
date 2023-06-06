{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Models (HTML (..), RawHtml (..), AuthInput (..), JwtTokens (..), RefreshInput (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (mimeRender))
import Data.OpenApi (ToSchema)

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: L.ByteString}

data AuthInput = AuthInput
  { username :: String,
    password :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON AuthInput

instance FromJSON AuthInput

instance ToSchema AuthInput

data JwtTokens = JwtTokens
  { access :: String,
    refresh :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON JwtTokens

instance FromJSON JwtTokens

instance ToSchema JwtTokens

newtype RefreshInput = RefreshInput
  { refresh :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON RefreshInput

instance FromJSON RefreshInput

instance ToSchema RefreshInput
