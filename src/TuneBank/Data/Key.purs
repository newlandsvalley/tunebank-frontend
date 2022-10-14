module TuneBank.Data.Key where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (slice)

newtype Key = Key String

derive instance newtypeKey :: Newtype Key _
derive instance genericKey :: Generic Key _
derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key
keys :: Array String
keys =
  [ "any"
  , "Adorian"
  , "Amajor"
  , "Aminor"
  , "Amixolydian"
  , "Bdorian"
  , "Bminor"
  , "Bmixolydian"
  , "Cdorian"
  , "Cmajor"
  , "Ddorian"
  , "Dmajor"
  , "Dminor"
  , "Dmixolydian"
  , "Edorian"
  , "Emajor"
  , "Eminor"
  , "Emixolydian"
  , "Fdorian"
  , "Fmajor"
  , "Gdorian"
  , "Gmajor"
  , "Gminor"
  , "Gmixolydian"
  ]

keySearchTerm' :: Key -> Maybe String
keySearchTerm' (Key k) =
  case k of
    "any" ->
       Nothing
    _ ->
       Just $ slice 0 4 k

keySearchTerm :: String -> Maybe String
keySearchTerm k =
  case k of
    "any" ->
       Nothing
    _ ->
       Just $ slice 0 4 k
