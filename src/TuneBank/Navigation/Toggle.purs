module TuneBank.Navigation.Toggle where

-- | The hamburger state is maintained entirely within css attributes on the appropriate HTNL elements.
-- | These attributes are reset to initial values (off) or toggled here

import Prelude (Unit)
import Effect (Effect)

-- | reset the visibility of the hamburger to its (default) starting value
-- | no menu is visible and the menu bar resorts to the hamburger lines (menu available)
foreign import resetHamburgerMenu :: Effect Unit

-- | toggle the visibility of the hamburger
-- | the menu bar toggles between hamburger lines (available) and hamburger cross (unavailable)
-- | and the menu tuggles between invisible and invisible in sync with the menu bar
foreign import toggleHamburgerMenu :: Effect Unit