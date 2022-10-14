module TuneBank.Data.TuneId
  ( TuneId(..)
  , tuneIdFromString
  , tuneIdToString
  , decodeTuneIdURIComponent
  , encodeTuneIdURIComponent
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String (replaceAll)
import Data.String.CodePoints (lastIndexOf, length)
import Data.String.CodeUnits (slice)
import Data.String.Pattern (Pattern(..), Replacement(..))
import TuneBank.Api.Codec.Utils (unsafeDecodeURIComponent, unsafeEncodeURIComponent, safeSlice)

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
  case lastIndexOf (Pattern "-") s of
    Just ix ->
      let
        title = slice 0 ix s
        tuneType = slice (ix + 1) (length s) s
      in
        Right $ TuneId $ {title, tuneType}
    _ ->
      Left $ "Not a TuneId: " <> s

-- | decode a tuneId coming back from the MusicRest server
-- | This needs to be URI decoded but seems to leave plus signs in place
-- | instead of spaces.  I don't yet know the reason
decodeTuneIdURIComponent :: String -> TuneId
decodeTuneIdURIComponent s =
  let
    decodedS :: String
    decodedS = unsafeDecodeURIComponent s
  in
    case lastIndexOf (Pattern "-") decodedS of
      Just ix ->
        TuneId { title : cleanTuneTitle $ safeSlice 0 ix decodedS
               , tuneType : safeSlice (ix + 1) (length decodedS) decodedS
               }
      Nothing ->
        TuneId { title : cleanTuneTitle decodedS
               , tuneType : ""
               }

-- | this is a hack to tidy up tune titles retruned from tune lists in musicrest
cleanTuneTitle :: String -> String
cleanTuneTitle =
  replaceAll (Pattern "+") (Replacement " ")

encodeTuneIdURIComponent :: TuneId -> String
encodeTuneIdURIComponent =
  unsafeEncodeURIComponent <<< tuneIdToString
