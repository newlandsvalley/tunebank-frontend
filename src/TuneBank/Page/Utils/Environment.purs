module TuneBank.Page.Utils.Environment where

import Prelude


import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Genre (Genre)

-- | get user
getUser
  :: forall m r
   . MonadEffect m
  => MonadAsk { session :: Session | r } m
  -- => Navigate m
  => m (Maybe Credentials)
getUser = do
  session <- asks _.session
  user <- (Ref.read >>> liftEffect) session.user
  pure user

-- | get the base URL
getBaseURL
  :: forall m r
   . MonadEffect m
  => MonadAsk { baseURL :: BaseURL | r } m
  -- => Navigate m
  => m BaseURL
getBaseURL =
  asks _.baseURL

-- | get the current genre
getCurrentGenre
  :: forall m r
   . MonadEffect m
  => MonadAsk { session :: Session | r } m
  -- => Navigate m
  => m Genre
getCurrentGenre = do
  session <- asks _.session
  genre <- (Ref.read >>> liftEffect) session.genre
  pure genre
