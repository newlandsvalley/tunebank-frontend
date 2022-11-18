module TuneBank.Data.UserId 
  ( UserId(..)
  , userIdFromString
  , userIdToString
  ) where

import Prelude
import Data.Either (Either(..))

newtype UserId = UserId String

derive newtype instance Eq UserId
derive newtype instance Ord UserId

instance showUserid :: Show UserId where
  show = userIdToString
  
userIdToString :: UserId -> String
userIdToString (UserId name) =
  name
  
userIdFromString :: String -> Either String UserId
userIdFromString s =
  Right (UserId s)