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
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..))
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

instance boundedGenre :: Bounded Genre where
  top = Scottish
  bottom = Irish

instance enumGenre :: Enum Genre where
  succ = succ
  pred = pred

instance boundedEnumGenre :: BoundedEnum Genre where
  cardinality = Cardinality 4
  toEnum = toEnum
  fromEnum = fromEnum

readGenre :: String -> Maybe Genre
readGenre genreStr =
  case genreStr of
    "Irish"    -> Just Irish
    "Klezmer"  -> Just Klezmer
    "Scandi"   -> Just Scandi
    "Scottish" -> Just Scottish
    "irish"    -> Just Irish
    "klezmer"  -> Just Klezmer
    "scandi"   -> Just Scandi
    "scottish" -> Just Scottish
    _ -> Nothing

toEnum :: Int -> Maybe Genre
toEnum i =
 case i of
    0  -> Just Irish
    1  -> Just Klezmer
    2  -> Just Scandi
    3  -> Just Scottish
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

fromEnum :: Genre -> Int
fromEnum genre =
  case genre of
    Irish    -> 0
    Klezmer  -> 1
    Scandi   -> 2
    Scottish -> 3

succ :: Genre -> Maybe Genre
succ genre =
  case genre of
    Irish    -> Just Klezmer
    Klezmer  -> Just Scandi
    Scandi   -> Just Scottish
    Scottish -> Nothing

pred :: Genre -> Maybe Genre
pred genre =
  case genre of
    Irish    -> Nothing
    Klezmer  -> Just Irish
    Scandi   -> Just Klezmer
    Scottish -> Just Scandi

asUriComponent :: Genre -> String
asUriComponent =
  toLower <<< show
