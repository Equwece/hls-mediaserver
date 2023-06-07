{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Users.APISpec where

import API.Users.Models (User)
import Data.UUID (UUID)
import Servant
  ( Capture,
    DeleteNoContent,
    Get,
    JSON,
    Post,
    Raw,
    ReqBody,
    type (:<|>),
    type (:>),
  )

type UserAPI =
  "users"
    :> ( Get '[JSON] [User]
           :<|> ReqBody '[JSON] User :> Post '[JSON] UUID
           :<|> UserEntityAPI
       )

type UserEntityAPI =
  Capture "userId" UUID
    :> ( Get '[JSON] User
           :<|> DeleteNoContent
           :<|> ReqBody '[JSON] User :> Post '[JSON] User
       )

type StaticIndexAPI = "index" :> Raw

type StaticSegmentsAPI = "segments" :> Raw
