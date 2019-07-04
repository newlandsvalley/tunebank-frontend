module TuneBank.Data.Session (Session) where

import Data.Maybe (Maybe)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import Effect.Ref (Ref)
import Audio.SoundFont (Instrument)

-- | session information for the user
-- | (who may or may not be logged in)
type Session =
  { user :: Ref (Maybe Credentials)
  , genre :: Ref Genre
  , instruments :: Ref (Array Instrument)
  }
