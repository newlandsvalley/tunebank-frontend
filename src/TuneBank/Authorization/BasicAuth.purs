module TuneBank.Authorization.BasicAuth
  (authorizationHeader) where


import Prelude ((<>), map)
import Affjax.RequestHeader (RequestHeader(..))
import Data.String.Base64 (encode)
import Data.Maybe (Maybe)
import TuneBank.Data.Credentials (Credentials)

authorizationHeader :: Maybe Credentials -> Maybe RequestHeader
authorizationHeader mcred =
  map authorizationHeader' mcred

authorizationHeader' :: Credentials -> RequestHeader
authorizationHeader' credentials =
  let
    b64 = encode (credentials.user <> ":" <> credentials.pass)
  in
    RequestHeader "Authorization" ("Basic " <> b64)
