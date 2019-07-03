module TuneBank.Api.Codec.Utils
  ( encodeURIComponent
  , decodeURIComponent
  , safeSlice) where

import Prelude ((<<<), ($))
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable as Nullable
import Data.String.CodeUnits (slice)

foreign import encodeURIComponent :: String -> String

foreign import decodeURIComponentImpl :: String -> Nullable.Nullable String

decodeURIComponent :: String -> Maybe String
decodeURIComponent = Nullable.toMaybe <<< decodeURIComponentImpl

safeSlice :: Int -> Int -> String -> String
safeSlice from to str =
  fromMaybe "" $ slice from to str
