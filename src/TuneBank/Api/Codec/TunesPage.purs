module TuneBank.Api.Codec.TunesPage
  ( TunesPage(..)
  , TuneRefArray
  , TuneRef
  , decodeTunesPage) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either)
import Data.Traversable (traverse)
import TuneBank.Api.Codec.Pagination (Pagination, decodeJsonPagination)

type TuneRef =
  { uri :: String
  , date :: String
  , abc :: String
  }

decodeJsonTuneRef :: Json -> Either String TuneRef
decodeJsonTuneRef json = do
    obj <- decodeJson json
    uri <- obj .: "uri"
    date <- obj .: "date"
    abc <- obj .: "abc"
    pure $ { uri, date, abc }

type TunesPage =
  { tunes :: TuneRefArray
  , pagination :: Pagination
  }

type TuneRefArray = Array TuneRef

decodeTuneRefArray :: Json -> Either String TuneRefArray
decodeTuneRefArray json = decodeJson json >>= traverse decodeJsonTuneRef

decodeTunesPage :: Json -> Either String TunesPage
decodeTunesPage json = do
  obj <- decodeJson json
  tunes <- obj .: "tunes" >>= decodeTuneRefArray
  pagination <- obj .: "pagination" >>= decodeJsonPagination
  pure $ { tunes, pagination }
