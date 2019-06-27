module TuneBank.Data.Credentials
  ( Credentials
  , Role(..) 
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
