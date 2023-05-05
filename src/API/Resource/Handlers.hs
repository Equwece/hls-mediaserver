{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Resource.Handlers (resultServer) where

import API.APISpec (RestAPI)
import API.External.Postgres (PostgresClass (getResourceByIdQuery, listResourcesQuery))
import API.Interfaces (AppEnvironment (AppEnvironment, db, logger), Logger (logMsg))
import API.Models (RawHtml (RawHtml))
import API.Resource.Models (Resource)
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Cont (MonadIO (liftIO))
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.OpenApi (HasInfo (info), HasLicense (license), HasServers (servers), HasTitle (title), HasVersion (version), OpenApi)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant (Application, Handler, Proxy (Proxy), Server, ServerT, Tagged, err404, serveDirectoryWebApp, throwError, type (:<|>) (..))
import Servant.API (Raw)
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Swagger.UI (SwaggerSchemaUI, SwaggerSchemaUI', swaggerSchemaUIServer)

resultServer appEnv = clientAppServer appEnv :<|> staticServer :<|> restApiServer appEnv

restApiServer appEnv = resourceServer appEnv :<|> swaggerServer

swaggerServer :: Server (SwaggerSchemaUI api b)
swaggerServer = swaggerSchemaUIServer openApiSpec

clientAppServer :: AppEnvironment -> [Text] -> Handler RawHtml
clientAppServer appEnv@(AppEnvironment {..}) appPath = do
  appHtml <- liftIO $ readFile "./templates/index.html"
  return . RawHtml $ fromString appHtml

resourceServer :: AppEnvironment -> Handler [Resource] :<|> (UUID -> Handler Resource :<|> (Tagged Handler Application :<|> Tagged Handler Application))
resourceServer appEnv@(AppEnvironment {..}) = listResources :<|> resourceEntityServer appEnv
  where
    listResources :: Handler [Resource]
    listResources = do
      resources <- liftIO $ listResourcesQuery db
      liftIO $ logMsg logger "List Resources"
      return resources

resourceEntityServer :: AppEnvironment -> UUID -> Handler Resource :<|> (Tagged Handler Application :<|> Tagged Handler Application)
resourceEntityServer (AppEnvironment {..}) resId = getResource resId :<|> staticSegmentServer resId :<|> staticSegmentServer resId
  where
    getResource :: UUID -> Handler Resource
    getResource rId = do
      res <- liftIO $ getResourceByIdQuery db rId
      liftIO $ logMsg logger ("Get Resource " <> show rId)
      if not (null res)
        then return (head res)
        else throwError err404

staticSegmentServer :: UUID -> ServerT Raw Handler
staticSegmentServer _ = serveDirectoryWebApp "./segments/"

staticServer :: ServerT Raw Handler
staticServer = serveDirectoryWebApp "./static/"

openApiSpec :: OpenApi
openApiSpec =
  toOpenApi (Proxy :: Proxy RestAPI)
    & info . title .~ "Mediaserver API"
    & info . version .~ "1.0"
    & info . license ?~ "LGPL"
