module TuneBank.Api.Codec.TunesPage
  ( TunesPage(..)
  , TuneRefArray
  , TuneRef
  , decodeTunesPage) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either)
import Data.Traversable (traverse)
import TuneBank.Api.Codec.Pagination (Pagination, decodeJsonPagination)

type TuneRef =
  { uri :: String
  , ts :: String
  , abc :: String
  }

decodeJsonTuneRef :: Json -> Either JsonDecodeError TuneRef
decodeJsonTuneRef json = do
    obj <- decodeJson json
    uri <- obj .: "uri"
    ts <- obj .: "ts"
    abcHeaders <- obj .: "abcHeaders"
    abcBody <- obj .: "abc"
    let
      abc = abcHeaders <> abcBody
    pure $ { uri, ts, abc }

type TunesPage =
  { tunes :: TuneRefArray
  , pagination :: Pagination
  }

type TuneRefArray = Array TuneRef

decodeTuneRefArray :: Json -> Either JsonDecodeError TuneRefArray
decodeTuneRefArray json = decodeJson json >>= traverse decodeJsonTuneRef

decodeTunesPage :: Json -> Either  JsonDecodeError TunesPage
decodeTunesPage json = do
  obj <- decodeJson json
  tunes <- obj .: "tunes" >>= decodeTuneRefArray
  pagination <- obj .: "pagination" >>= decodeJsonPagination
  pure $ { tunes, pagination }
