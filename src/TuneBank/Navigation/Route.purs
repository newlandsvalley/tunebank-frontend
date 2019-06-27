module TuneBank.Navigation.Route where

-- | Routes - TuneBank URLs which are allowable and provide a round trip between
-- | each Route and a stringified version suitable for a URL


import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', optional, root, segment, string, int, as)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import TuneBank.Data.Types (TuneId, tuneIdFromString, tuneIdToString)
import TuneBank.Navigation.SearchParams (SearchParams)


tuneId :: RouteDuplex' String -> RouteDuplex' TuneId
tuneId = as tuneIdToString tuneIdFromString

data Route
  = Home
  | SearchForm
  | Genre
  | Login
  | Register
  | Upload
  | Tune String TuneId
  | TuneList SearchParams

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "SearchForm" : noArgs
  , "Login": "login" / noArgs
  , "Genre": noArgs
  , "Register": "register" / noArgs
  , "Upload": "upload" / noArgs
  , "Tune": "genre" / segment / "tune" / (tuneId segment)
  , "TuneList":  "tunelist" ?
       { key : optional <<< string
       , rhythm : optional <<< string
       , title : optional <<< string
       , source : optional <<< string
       , transcriber : optional <<< string
       , abc : optional <<< string
       , page: int
       , sort : string }
  }
