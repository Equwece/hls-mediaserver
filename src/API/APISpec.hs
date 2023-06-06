{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.APISpec where

import API.Models (AuthInput, HTML (..), JwtTokens, RawHtml (..), RefreshInput)
import API.Resource.APISpec (ResourceAPI)
import Data.Text (Text)
import Servant (CaptureAll, Get, JSON, Post, Proxy (Proxy), Raw, ReqBody, type (:<|>), type (:>))
import Servant.Swagger.UI (SwaggerSchemaUI)

type API =
  "app" :> CaptureAll "appPath" Text :> Get '[HTML] RawHtml
    :<|> "static" :> Raw
    :<|> "v1" :> (RestAPI :<|> SwaggerAPI)

type SwaggerAPI =
  SwaggerSchemaUI "swagger-ui" "swagger.json"

type RestAPI = ResourceAPI :<|> AuthAPI

type AuthAPI =
  "auth"
    :> ( "create" :> ReqBody '[JSON] AuthInput :> Post '[JSON] JwtTokens
           :<|> "refresh" :> ReqBody '[JSON] RefreshInput :> Post '[JSON] JwtTokens
       )

proxyAPI :: Proxy API
proxyAPI = Proxy
