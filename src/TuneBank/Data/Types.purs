module TuneBank.Data.Types
  ( LogLevel(..)
  , BaseURL(..)
  , Env
  ) where

import Prelude
import TuneBank.Data.Session (Session)

-- | A flag to control the environment for logging messages.
data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

-- | the Base URL of the downstream server providing the API
newtype BaseURL = BaseURL String

instance showBaseURL :: Show BaseURL where
  show (BaseURL s) = s

-- | the environment of the app made available to ReaderT
type Env =
  { logLevel :: LogLevel
  , baseURL :: BaseURL
  , session :: Session
  }
