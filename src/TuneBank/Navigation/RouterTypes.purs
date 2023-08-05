module TuneBank.Navigation.RouterTypes where

-- | we remove the Router Action to its own module so that it can be shared with Headers

import Audio.SoundFont (Instrument)

type Input =
  { instruments :: Array Instrument }

data Action
  = Initialize
  | HandleInput Input
  | ToggleHamburgerMenu
