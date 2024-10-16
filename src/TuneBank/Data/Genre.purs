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
    English
  | Irish
  | Klezmer
  | Scandi
  | Scottish

instance showGenre :: Show Genre where
  show English  = "English"
  show Irish    = "Irish"
  show Klezmer  = "Klezmer"
  show Scandi   = "Scandi"
  show Scottish = "Scottish"

derive instance eqGenre :: Eq Genre
derive instance ordGenre :: Ord Genre

instance boundedGenre :: Bounded Genre where
  top = Scottish
  bottom = English

instance enumGenre :: Enum Genre where
  succ = succ
  pred = pred

instance boundedEnumGenre :: BoundedEnum Genre where
  cardinality = Cardinality 5
  toEnum = toEnum
  fromEnum = fromEnum

readGenre :: String -> Maybe Genre
readGenre genreStr =
  case genreStr of
    "English"  -> Just English
    "Irish"    -> Just Irish
    "Klezmer"  -> Just Klezmer
    "Scandi"   -> Just Scandi
    "Scottish" -> Just Scottish
    "english"  -> Just English
    "irish"    -> Just Irish
    "klezmer"  -> Just Klezmer
    "scandi"   -> Just Scandi
    "scottish" -> Just Scottish
    _ -> Nothing

toEnum :: Int -> Maybe Genre
toEnum i =
 case i of
    0  -> Just English
    1  -> Just Irish
    2  -> Just Klezmer
    3  -> Just Scandi
    4  -> Just Scottish
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
    English  -> 0
    Irish    -> 1
    Klezmer  -> 2
    Scandi   -> 3
    Scottish -> 4

succ :: Genre -> Maybe Genre
succ genre =
  case genre of
    English  -> Just Irish
    Irish    -> Just Klezmer
    Klezmer  -> Just Scandi
    Scandi   -> Just Scottish
    Scottish -> Nothing

pred :: Genre -> Maybe Genre
pred genre =
  case genre of
    English  -> Nothing
    Irish    -> Just English
    Klezmer  -> Just Irish
    Scandi   -> Just Klezmer
    Scottish -> Just Scandi

asUriComponent :: Genre -> String
asUriComponent =
  toLower <<< show
