module TuneBank.Api.Codec.Utils
  ( containsDigit
  , encodeURIComponent
  , decodeURIComponent
  , safeSlice) where

import Prelude ((<<<), ($), (>=), (<=), (&&))
import Data.Maybe (Maybe)
import Data.Foldable (any)
import Data.Nullable as Nullable
import Data.String.CodeUnits (slice)
import Data.String.CodePoints (CodePoint, codePointFromChar, toCodePointArray)

foreign import encodeURIComponent :: String -> String

foreign import decodeURIComponentImpl :: String -> Nullable.Nullable String

decodeURIComponent :: String -> Maybe String
decodeURIComponent = Nullable.toMaybe <<< decodeURIComponentImpl

safeSlice :: Int -> Int -> String -> String
safeSlice from to str =
  slice from to str

isDigit :: CodePoint -> Boolean
isDigit cp =
  cp >= codePointFromChar '0' && cp <= codePointFromChar '9'

-- | return true if the string contains any digit in the range 0-9
containsDigit :: String -> Boolean
containsDigit s =
  any isDigit $ toCodePointArray s
