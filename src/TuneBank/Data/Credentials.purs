module TuneBank.Data.Credentials
  ( Credentials
  , Role(..)
  , blankCredentials
  ) where

-- | the user role
data Role =
    NormalUser
  | Administrator

-- | the credentials of a user
type Credentials =
  { user :: String
  , pass :: String
  , role :: Role
  }

-- | blanked out (uninitialised) credentials
blankCredentials :: Credentials
blankCredentials =
  { user : ""
  , pass : ""
  , role : NormalUser
  }
