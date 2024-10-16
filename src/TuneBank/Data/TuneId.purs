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
  case indexOfRhythmSeparator s of
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
    case indexOfRhythmSeparator decodedS of
      Just ix ->
        TuneId { title : cleanPlusCharacters $ safeSlice 0 ix decodedS
               , tuneType : cleanPlusCharacters $ safeSlice (ix + 1) (length decodedS) decodedS
               }
      Nothing ->
        TuneId { title : cleanPlusCharacters decodedS
               , tuneType : ""
               }

-- | this is a hack to tidy up any + characters left in the URI string
cleanPlusCharacters :: String -> String
cleanPlusCharacters=
  replaceAll (Pattern "+") (Replacement " ")

encodeTuneIdURIComponent :: TuneId -> String
encodeTuneIdURIComponent =
  unsafeEncodeURIComponent <<< tuneIdToString

-- | the rhythm is separated from the title in the URI by the final dash except for three-twos!
indexOfRhythmSeparator :: String -> Maybe Int
indexOfRhythmSeparator s = 
  case lastIndexOf (Pattern "-three-two") s of
    Just ix ->
      Just ix
    _ ->
      lastIndexOf (Pattern "-") s
