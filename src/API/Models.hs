{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Models (HTML (..), RawHtml (..)) where

import Data.ByteString.Lazy as L
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (mimeRender))

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: L.ByteString}
