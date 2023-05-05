{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.APISpec where

import API.Models (HTML (..), RawHtml (..))
import API.Resource.APISpec (ResourceAPI)
import Data.Text (Text)
import Servant (CaptureAll, Get, JSON, Proxy (Proxy), Raw, type (:<|>), type (:>))
import Servant.Swagger.UI (SwaggerSchemaUI)

type API =
  "app" :> CaptureAll "appPath" Text :> Get '[HTML] RawHtml
    :<|> "static" :> Raw
    :<|> "v1" :> (RestAPI :<|> SwaggerAPI)

type SwaggerAPI =
  SwaggerSchemaUI "swagger-ui" "swagger.json"

type RestAPI = ResourceAPI

proxyAPI :: Proxy API
proxyAPI = Proxy
