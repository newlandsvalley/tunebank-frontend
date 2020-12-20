module TuneBank.Api.Codec.Pagination
  ( Pagination
  , decodeJsonPagination) where

-- | Pagination responses are now held solely in the returned JSON for
-- | Tune and User lists

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either)

type Pagination =
  { page :: Int
  , size :: Int
  , maxPages :: Int
  }

-- | decode the JSON results of a search request page number

decodeJsonPagination :: Json -> Either JsonDecodeError Pagination
decodeJsonPagination json = do
   obj <- decodeJson json
   page <- obj .: "page"
   size <- obj .: "size"
   maxPages <- obj .: "maxPages"
   pure $ { page, size, maxPages }
