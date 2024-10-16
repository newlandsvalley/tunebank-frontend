module TuneBank.Data.Rhythm where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import TuneBank.Data.Genre (Genre(..))

newtype Rhythm = Rhythm String

derive instance newtypeRhythm :: Newtype Rhythm _
derive instance genericRhythm :: Generic Rhythm _
derive instance eqRhythm :: Eq Rhythm
derive instance ordRhythm :: Ord Rhythm

rhythms :: Genre -> Array String
rhythms genre =
  case genre of
    English ->
      englishRhythms
    Scandi ->
      scandiRhythms
    Irish ->
      irishRhythms
    Scottish ->
      scottishRhythms
    Klezmer ->
      klezmerRhythms


scandiRhythms :: Array String
scandiRhythms =
  [ "any"
  , "polska"
  , "slängpolska"
  , "marsch"
  , "brudmarsch"
  , "gånglåt"
  , "skänklåt"
  , "polka"
  , "långdans"
  , "schottis"
  , "engelska"
  , "halling"
  , "hambo"
  , "sekstur"
  , "waltz"
  ]

klezmerRhythms :: Array String
klezmerRhythms =
  [ "any"
  , "bulgar"
  , "freylekhs"
  , "khosidl"
  , "hora"
  , "csardas"
  , "doina"
  , "honga"
  , "hopak"
  , "kasatchok"
  , "kolomeyke"
  , "sher"
  , "sirba"
  , "skotshne"
  , "taksim"
  , "terkish"
  ]

celticRhythms :: Array String
celticRhythms =
  [ "any"
  , "jig"
  , "reel"
  , "hornpipe"
  , "barndance"
  , "march"
  , "polka"
  , "slip jig"
  , "waltz"
  ]

irishRhythms :: Array String
irishRhythms =
  celticRhythms <>
  [ "highland"
  , "mazurka"
  , "slide"
  ]

scottishRhythms :: Array String
scottishRhythms =
  celticRhythms <>
  [ "schottische"
  , "strathspey"
  ]

englishRhythms :: Array String
englishRhythms =
  [ "any"
  , "jig"
  , "reel"
  , "hornpipe"
  , "march"
  , "minuet"
  , "polka"
  , "three-two"
  , "waltz"
  ]