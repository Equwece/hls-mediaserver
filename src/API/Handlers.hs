{-# LANGUAGE DataKinds #-}

module API.Handlers (apiServer) where

import API.APISpec (API)
import API.Interfaces (AppEnvironment)
import API.Resource.Handlers (resultServer)
import Servant (Server)

apiServer :: AppEnvironment -> Server API
apiServer = resultServer
