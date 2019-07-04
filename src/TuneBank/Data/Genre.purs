module TuneBank.Data.Genre
  ( Genre(..)
  , asUriComponent
  , readGenre
  , genreFromString
  , genreToString
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String.Common (toLower)

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

genreToString :: Genre -> String
genreToString =
  asUriComponent

genreFromString :: String -> Either String Genre
genreFromString s =
  case (readGenre s) of
    Just genre ->
      Right genre
    Nothing ->
      Left $ "Not a Genre: " <> s

asUriComponent :: Genre -> String
asUriComponent =
  toLower <<< show
