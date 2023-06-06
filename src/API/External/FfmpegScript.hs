{-# LANGUAGE OverloadedStrings #-}

module API.External.FfmpegScript (ffmpegScript) where

import Data.Text (Text, pack)
import Data.UUID (UUID, toString)
import Turtle (Alternative (empty), inshell, stdout)

ffmpegScript :: (UUID, FilePath) -> IO ()
ffmpegScript (resId, mediaFilePath) = do
  putStrLn mediaFilePath
  stdout
    ( inshell (makeFfmpegLine mediaFilePath resId) empty
    )

makeFfmpegLine :: FilePath -> UUID -> Text
makeFfmpegLine mediaFilePath resId = resultLine
  where
    part1 = "ffmpeg -i \""
    part2 = "\" -bsf:v h264_mp4toannexb -codec copy -hls_list_size 0 "
    resultLine =
      pack $ part1 <> mediaFilePath <> part2 <> "./data/segments/index" <> toString resId <> ".m3u8"
