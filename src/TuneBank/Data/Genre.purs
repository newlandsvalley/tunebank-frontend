module TuneBank.Data.Genre
  ( Genre(..)
  , readGenre
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)

-- | the supported genres
data Genre = 
    Irish
  | Klezmer
  | Scandi
  | Scottish

instance showGenre :: Show Genre where
  show Irish    = "Irish"
  show Klezmer  = "Klezmer"
  show Scandi   = "Scandi"
  show Scottish = "Scottish"

derive instance eqGenre :: Eq Genre
derive instance ordGenre :: Ord Genre

readGenre :: String -> Maybe Genre 
readGenre genreStr =
  case genreStr of 
    "Irish"    -> Just Irish
    "Klezmer"  -> Just Klezmer
    "Scandi"   -> Just Scandi
    "Scottish" -> Just Scottish
    _ -> Nothing



