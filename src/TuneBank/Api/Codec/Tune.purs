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
  , source :: Maybe String
  , composer :: Maybe String
  , origin :: Maybe String
  , transcriber :: Maybe String
  , submitter :: String
  , tid :: String
  , ts :: String
  , abc :: String
  }

nullTuneMetadata :: TuneMetadata
nullTuneMetadata =
  { title : ""
  , source : Nothing
  , composer : Nothing
  , origin : Nothing
  , transcriber : Nothing
  , submitter : ""
  , tid : ""
  , ts : ""
  , abc : ""
  }

decodeTune :: Json -> Either String TuneMetadata
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
  
