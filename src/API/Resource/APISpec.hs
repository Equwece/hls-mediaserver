{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Resource.APISpec where

import API.Resource.Models (Resource)
import Data.UUID (UUID)
import Servant (AuthProtect, Capture, Get, JSON, Post, PostNoContent, Raw, type (:<|>) (..), type (:>))

type ResourceAPI =
  (PostNoContent)
    :<|> "resources"
      :> ( Get '[JSON] [Resource]
             :<|> "update" :>  ResourceEntityAPI
         )

type ResourceEntityAPI =
  Capture "resourceId" UUID
    :> ( Get '[JSON] Resource
           :<|> "index" :> Raw
           :<|> "segments" :> Raw
       )

type StaticIndexAPI = "index" :> Raw

type StaticSegmentsAPI = "segments" :> Raw
