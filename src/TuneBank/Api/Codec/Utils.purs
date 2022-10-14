module TuneBank.Api.Codec.Utils
  ( containsDigit
  , unsafeEncodeURIComponent
  , unsafeDecodeURIComponent
  , safeSlice) where

import Prelude (($), (>=), (<=), (&&))
import Data.Maybe (fromJust)
import Data.Foldable (any)
import Data.String.CodeUnits (slice)
import Data.String.CodePoints (CodePoint, codePointFromChar, toCodePointArray)
import Partial.Unsafe (unsafePartial)
import JSURI (decodeURIComponent, encodeURIComponent)

unsafeEncodeURIComponent :: String -> String
unsafeEncodeURIComponent s = 
  unsafePartial $ fromJust $ encodeURIComponent s

unsafeDecodeURIComponent :: String -> String
unsafeDecodeURIComponent s = 
  unsafePartial $ fromJust $ decodeURIComponent s

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
