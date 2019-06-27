module TuneBank.Navigation.Endpoint
  (PageParams, Endpoint(..), endpointCodec)  where

-- | Endpoints - legitimate URLs at he musicrest backend


import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root, segment, as, int, optional, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import TuneBank.Data.Types (TuneId, tuneIdFromString, tuneIdToString)
import TuneBank.Navigation.SearchParams (SearchParams)

type PageParams =
  { page :: Int }

tuneId :: RouteDuplex' String -> RouteDuplex' TuneId
tuneId = as tuneIdToString tuneIdFromString

data Endpoint
  = Search String SearchParams
  | Users PageParams
  | UserCheck
  | Register
  | Tune String TuneId
  | Comments String TuneId

derive instance genericEndpoint :: Generic Endpoint _
derive instance eqEndpoint :: Eq Endpoint
derive instance ordEndpoint :: Ord Endpoint

instance showEndpoint :: Show Endpoint where
  show = genericShow


-- | Our codec will cause a compile-time error if we fail to handle any of our endpoint cases.
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Search": "genre" / segment / "search" ?
       { key : optional <<< string
       , rhythm : optional <<< string
       , title : optional <<< string
       , source : optional <<< string
       , transcriber : optional <<< string
       , abc : optional <<< string
       , page: int
       , sort : string }
  , "Users": "user" ? { page: int }
  , "UserCheck": "user" / "check" / noArgs
  , "Register": "register" / noArgs
  , "Tune": "genre" / segment / "tune" / (tuneId segment)
  , "Comments": "genre" / segment / "tune" / (tuneId segment) / "comments"
  }
