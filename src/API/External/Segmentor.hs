{-# LANGUAGE RecordWildCards #-}

module API.External.Segmentor (segmentContent) where

import API.External.FfmpegScript (ffmpegScript)
import API.External.Postgres (PostgresClass (addResource, getResourceByHash))
import API.Interfaces (AppEnvironment (AppEnvironment, db))
import API.Resource.Models (Resource (Resource, isSegmented, resourceHash, resourceId, resourceTitle, resourceType), ResourceType (Video))
import Control.Monad (forM, forM_)
import Crypto.Hash.SHA256 (hash, hashlazy)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.ByteString.Lazy as LB
import Data.UUID (nil)
import System.Directory (listDirectory)

segmentContent :: AppEnvironment -> IO ()
segmentContent appEnv = do
  sourceFiles <- listDirectory "./data/source"
  sourceTuple <- filter (\(i, _) -> i /= nil) <$> forM sourceFiles (prepareContent appEnv)
  forM_ (map (second ("./data/source/" <>)) sourceTuple) ffmpegScript

prepareContent AppEnvironment {..} mediaFileName = do
  fileContent <- LB.readFile ("./data/source/" <> mediaFileName)
  let fileHash = show . hashlazy $ fileContent
  maybeResource <- getResourceByHash db fileHash
  if null maybeResource
    then do
      let newResource =
            Resource
              { resourceId = nil,
                resourceType = Video,
                resourceTitle = mediaFileName,
                isSegmented = True,
                resourceHash = fileHash
              }
      newResourceId <- head <$> addResource db newResource
      return (newResourceId, mediaFileName)
    else return (nil, mediaFileName)
