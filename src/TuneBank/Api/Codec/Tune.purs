module TuneBank.Api.Codec.Tune
  ( TuneMetadata(..)
  , nullTuneMetadata
  , decodeTune) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Either (Either)
import Data.Maybe (Maybe(..))

type TuneMetadata =
  { title :: String
  , rhythm :: String
  , source :: Maybe String
  , composer :: Maybe String
  , origin :: Maybe String
  , transcriber :: Maybe String
  , submitter :: String
  , ts :: String
  , abc :: String
  }

nullTuneMetadata :: TuneMetadata
nullTuneMetadata =
  { title : ""
  , rhythm : ""
  , source : Nothing
  , composer : Nothing
  , origin : Nothing
  , transcriber : Nothing
  , submitter : ""
  , ts : ""
  , abc : ""
  }


-- | this is what's provided by the Haskell backend
decodeTune :: Json -> Either String TuneMetadata
decodeTune json = do
  obj <- decodeJson json
  title <- obj .: "title"
  rhythm <- obj .: "rhythm"
  source <- obj .:? "source"
  composer <- obj .:? "composer"
  origin <- obj .:? "origin"
  transcriber <- obj .:? "transcriber"
  submitter <- obj .: "submitter"
  abc <- obj .: "abc"
  -- ts <- obj .: "ts"
  let
    ts = "timestamp"
  pure $ { title, rhythm, source, composer, origin, transcriber, submitter, ts, abc }

{-
-- | this is what's provided by the Scala backend
decodeTune json = do
  obj <- decodeJson json
  title <- obj .: "T"
  source <- obj .:? "S"
  composer <- obj .:? "C"
  origin <- obj .:? "O"
  transcriber <- obj .:? "Z"
  submitter <- obj .: "submitter"
  tid <- obj .: "tid"
  abc <- obj .: "abc"
  ts <- obj .: "ts"
  pure $ { title, source, composer, origin, transcriber, submitter, tid, ts, abc }
-}
