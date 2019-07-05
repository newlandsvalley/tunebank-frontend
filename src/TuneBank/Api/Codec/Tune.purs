module TuneBank.Api.Codec.Tune
  ( TuneMetadata(..)
  , nullTuneMetadata
  , fixJson
  , decodeTune) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodePoints (lastIndexOf, length)
import TuneBank.Api.Codec.Utils (safeSlice)

type TuneMetadata =
  { title :: String
  , submitter :: String
  , tid :: String
  , ts :: String
  , abc :: String
  }

nullTuneMetadata :: TuneMetadata
nullTuneMetadata =
  { title : ""
  , submitter : ""
  , tid : ""
  , ts : ""
  , abc : ""
  }

-- | Fix a bug in the JSON returned by MusicRest.  newlines in the ABC string (one of the strings returned in the JSON)
-- | are unescaped.  The abc string is the final string and this method escapes them properly.
fixJson :: String -> String
fixJson s =
  case lastIndexOf (Pattern "\"") s  of
    Nothing ->
      s
    Just last ->
      case lastIndexOf (Pattern "\"") (safeSlice 0 last s) of
        Nothing ->
          s
        Just penultimate ->
          (safeSlice 0 penultimate s) <>
          replaceAll (Pattern "\n") (Replacement "\\n") (safeSlice penultimate last s) <>
          (safeSlice last (length s) s)


decodeJsonTune :: Json -> Either String TuneMetadata
decodeJsonTune json = do
  obj <- decodeJson json
  title <- obj .: "T"
  submitter <- obj .: "submitter"
  tid <- obj .: "tid"
  abc <- obj .: "abc"
  ts <- obj .: "ts"
  pure $ { title, submitter, tid, ts, abc }


decodeTune :: Json -> Either String TuneMetadata
decodeTune json = do
  decodeJsonTune json
