{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API.APISpec where

import API.Models (Account, AuthInput, HTML (..), JwtTokens, RawHtml (..), RefreshInput)
import API.Resource.APISpec (ResourceAPI)
import API.Users.APISpec (UserAPI)
import Data.Text (Text)
import Servant (AuthProtect, CaptureAll, Get, Header, JSON, Post, Proxy (Proxy), Raw, ReqBody, type (:<|>), type (:>))
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Swagger.UI (SwaggerSchemaUI)

type API =
  "app" :> CaptureAll "appPath" Text :> Get '[HTML] RawHtml
    :<|> "static" :> Raw
    -- :<|> "v1" :> (RestAPI :<|> SwaggerAPI)
    :<|> "v1" :> (RestAPI)

type SwaggerAPI =
  SwaggerSchemaUI "swagger-ui" "swagger.json"

type RestAPI =
  -- (AuthProtect "jwt-auth" :> (ResourceAPI :<|> UserAPI)) :<|> AuthAPI
  ((AuthProtect "jwt-auth" :> ResourceAPI) :<|> (AuthProtect "jwt-auth" :> UserAPI)) :<|> AuthAPI

type instance AuthServerData (AuthProtect "jwt-auth") = Account

type AuthAPI =
  "auth"
    :> ( "create" :> ReqBody '[JSON] AuthInput :> Post '[JSON] JwtTokens
           :<|> "refresh" :> ReqBody '[JSON] RefreshInput :> Post '[JSON] JwtTokens
       )

proxyAPI :: Proxy API
proxyAPI = Proxy
