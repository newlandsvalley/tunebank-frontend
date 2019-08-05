module TuneBank.Api.Codec.Register
  ( Submission
  , defaultSubmission
  , encodeFormData ) where

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import TuneBank.Navigation.Route (Route(..), routeCodec)
import Routing.Duplex (print)
import Data.FormURLEncoded (FormURLEncoded, fromArray)

-- the form data to be posted in a new user registration
type Submission =
  { name :: String
  , email :: String
  , password :: String
  , password2 :: String
  , refererUrl :: String
  }

defaultSubmission :: Submission
defaultSubmission =
  { name : ""
  , email : ""
  , password : ""
  , password2 : ""
  , refererUrl : print routeCodec Register
  }

encodeFormData :: Submission -> FormURLEncoded
encodeFormData submission =
  fromArray
     [ Tuple "name"  (Just submission.name)
     , Tuple "email"  (Just submission.email)
     , Tuple "password"  (Just submission.password)
     , Tuple "password2"  (Just submission.password2)
     , Tuple "refererUrl"  (Just submission.refererUrl)
     ]
