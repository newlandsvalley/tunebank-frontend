module TuneBank.Api.Codec.TunesPage
  ( TunesPage(..)
  , TuneRefArray
  , TuneRef
  , decodeTunesPage) where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either)
import Data.Traversable (traverse)
import TuneBank.Api.Codec.Pagination (PageNum, decodeJsonPageNum)

type TuneRef =
  { uri :: String
  , ts :: String
  , abc :: String
  }

decodeJsonTuneRef :: Json -> Either String TuneRef
decodeJsonTuneRef json = do
    obj <- decodeJson json
    uri <- obj .: "uri"
    ts <- obj .: "ts"
    abcHeaders <- obj .: "abcHeaders"
    abcBody <- obj .: "abc"
    let
      abc = abcHeaders <> abcBody
    pure $ { uri, ts, abc }

type TunesPage =
  { tunes :: TuneRefArray
  , pageNum :: PageNum
  }

{-
instance showTunesPage :: Show TunesPage where
  show (TunesPage p) = show p.tunes <> " " <> show p.pageNum

instance eqTunesPage :: Eq TunesPage where
  eq (TunesPage p1) (TunesPage p2) =
    (p1.tunes == p2.tunes) &&
    (p1.pageNum == p2.pageNum)
-}

type TuneRefArray = Array TuneRef

decodeTuneRefArray :: Json -> Either String TuneRefArray
decodeTuneRefArray json = decodeJson json >>= traverse decodeJsonTuneRef

decodeTunesPage :: Json -> Either String TunesPage
decodeTunesPage json = do
  obj <- decodeJson json
  tunes <- obj .: "tune" >>= decodeTuneRefArray
  pageNum <- obj .: "pagination" >>= decodeJsonPageNum
  pure $ { tunes, pageNum }
