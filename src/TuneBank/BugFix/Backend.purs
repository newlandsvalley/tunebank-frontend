module TuneBank.BugFix.Backend where


import Prelude (($))
import Data.String (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import TuneBank.Navigation.Endpoint (Endpoint(..))

-- | Fix for bug in the musicrest backend - see:
-- | https://github.com/newlandsvalley/musicrest/issues/18
-- | search parameters in the URL are case-sensitibe
fixSearchParams :: Endpoint -> String -> String
fixSearchParams endpoint buggys =
  case endpoint of
    Search _ _ ->
      replace (Pattern "key") (Replacement "K") $
      replace (Pattern "rhythm") (Replacement "R") $
      replace (Pattern "title") (Replacement "T") $
      replace (Pattern "transcriber") (Replacement "Z") $
      replace (Pattern "source") (Replacement "S") buggys
    _ ->
      buggys
