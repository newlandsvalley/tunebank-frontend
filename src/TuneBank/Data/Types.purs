module TuneBank.Data.Types
  ( LogLevel(..)
  , BaseURL(..)
  , Env
  , TuneId(..)
  , tuneIdFromString
  , tuneIdToString
  ) where


import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.String.CodePoints (indexOf, length)
import Data.String.CodeUnits (slice)
import Data.String.Pattern (Pattern(..))
import TuneBank.Data.Session (Session)

-- | A flag to control the environment for logging messages.
data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

-- | the Base URL of the downstream server providing the API
newtype BaseURL = BaseURL String

-- | the environment of the app made available to ReaderT
type Env =
  { logLevel :: LogLevel
  , baseURL :: BaseURL
  , session :: Session
  }

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
