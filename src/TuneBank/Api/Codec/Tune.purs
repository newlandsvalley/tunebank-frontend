module TuneBank.Api.Codec.Tune
  ( Tune(..)
  , fixJson
  , decodeTune) where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, Json, encodeJson, decodeJson, (.:))
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodePoints (lastIndexOf, length)
import Data.String.CodeUnits (slice)

type Tune =
  { title :: String
  , submitter :: String
  , tid :: String
  , ts :: String
  , abc :: String
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
  where
    safeSlice :: Int -> Int -> String -> String
    safeSlice from to str =
      fromMaybe "" $ slice from to str


decodeJsonTune :: Json -> Either String Tune
decodeJsonTune json = do
  obj <- decodeJson json
  title <- obj .: "T"
  submitter <- obj .: "submitter"
  tid <- obj .: "tid"
  abc <- obj .: "abc"
  ts <- obj .: "ts"
  pure $ { title, submitter, tid, ts, abc }


decodeTune :: Json -> Either String Tune
decodeTune json = do
  decodeJsonTune json
