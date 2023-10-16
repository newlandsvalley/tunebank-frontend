module TuneBank.Page.Utils.Environment where

import Prelude


import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Data.Genre (Genre)
import Audio.SoundFont (Instrument)--

-- | while developing we'll sidestep CORS errors by querying through a
-- | CORS proxy server
corsAnywhere :: String
corsAnywhere =
  "https://cors-anywhere.herokuapp.com/"

-- | get user
getUser
  :: forall m r
   . MonadEffect m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => m (Maybe Credentials)
getUser = do
  session <- asks _.session
  user <- (Ref.read >>> liftEffect) session.user
  pure user

-- | get the base URL
getBaseURL
  :: forall m r
   . MonadEffect m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => m BaseURL
getBaseURL =
  asks _.baseURL

-- | get the base URL routed through cors anywhere
-- | this is only a temorary expedient whilst in development
getCorsBaseURL
  :: forall m r
   . MonadEffect m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => m BaseURL
getCorsBaseURL = do
  (BaseURL base) <- getBaseURL
  pure $ BaseURL (corsAnywhere <> base)


-- | get the current genre
getCurrentGenre
  :: forall m r
   . MonadEffect m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => m Genre
getCurrentGenre = do
  session <- asks _.session
  genre <- (Ref.read >>> liftEffect) session.genre
  pure genre

-- | get the instruments
getInstruments
  :: forall m r
   . MonadEffect m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
  => m (Array Instrument)
getInstruments= do
  session <- asks _.session
  instruments <- (Ref.read >>> liftEffect) session.instruments
  pure instruments

-- | get the cutoff viewport width that defines a small device (e.g. a mobile)
smallDeviceViewportWidthCutoff :: Int
smallDeviceViewportWidthCutoff = 725
