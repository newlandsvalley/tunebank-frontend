module TuneBank.Data.TuneId
  ( TuneId(..)
  , tuneIdFromString
  , tuneIdToString
  , decodeTuneIdURIComponent
  , encodeTuneIdURIComponent
  ) where


import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.String.CodePoints (indexOf, lastIndexOf, length)
import Data.String.CodeUnits (slice)
import Data.String.Pattern (Pattern(..))
import TuneBank.Api.Codec.Utils (decodeURIComponent, encodeURIComponent, safeSlice)

newtype TuneId = TuneId { title :: String, tuneType :: String }

derive instance genericTuneId :: Generic TuneId _
derive instance eqTuneId :: Eq TuneId
derive instance ordTuneId :: Ord TuneId

instance showTuneid :: Show TuneId where
  show = tuneIdToString

tuneIdToString :: TuneId -> String
tuneIdToString (TuneId { title, tuneType }) =
  title <> "-" <> tuneType

tuneIdFromString :: String -> Either String TuneId
tuneIdFromString s =
  case indexOf (Pattern "-") s of
    Just ix ->
      let
        mTitle = slice 0 ix s
        mTuneType = slice (ix + 1) (length s) s
      in
        case Tuple mTitle mTuneType of
          Tuple (Just title) (Just tuneType) ->
            Right $ TuneId $ {title, tuneType}
          _ ->
            Left $ "Not a TuneId: " <> s
    _ ->
      Left $ "Not a TuneId: " <> s

-- not yet safe
decodeTuneIdURIComponent :: String -> TuneId
decodeTuneIdURIComponent s =
  let
    decodedS :: String
    decodedS = fromMaybe "" $ decodeURIComponent s
  in
    case lastIndexOf (Pattern "-") decodedS of
      Just ix ->
        TuneId { title : safeSlice 0 ix decodedS
               , tuneType : safeSlice (ix + 1) (length decodedS) decodedS
               }
      Nothing ->
        TuneId { title : decodedS
               , tuneType : ""
               }

encodeTuneIdURIComponent :: TuneId -> String
encodeTuneIdURIComponent =
  encodeURIComponent <<< tuneIdToString
