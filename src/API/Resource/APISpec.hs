{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Resource.APISpec where

import API.Resource.Models (Resource)
import Data.UUID (UUID)
import Servant (Capture, Get, JSON, Post, PostNoContent, Raw, type (:<|>) (..), type (:>))

type ResourceAPI =
  ("update" :> PostNoContent)
    :<|> "resources"
      :> ( Get '[JSON] [Resource]
             :<|> ResourceEntityAPI
         )

type ResourceEntityAPI =
  Capture "resourceId" UUID
    :> ( Get '[JSON] Resource
           :<|> "index" :> Raw
           :<|> "segments" :> Raw
       )

type StaticIndexAPI = "index" :> Raw

type StaticSegmentsAPI = "segments" :> Raw
