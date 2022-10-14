module TuneBank.Navigation.Route where

-- | Routes - TuneBank URLs which are allowable and provide a round trip between
-- | each Route and a stringified version suitable for a URL


import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', optional, root, segment, string, int, as)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import TuneBank.Data.TuneId (TuneId, tuneIdFromString, tuneIdToString)
import TuneBank.Data.CommentId (CommentId, commentIdFromString, commentIdToString)
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Navigation.Endpoint (PageParams)
import TuneBank.Data.Genre (Genre, genreFromString, genreToString)

tuneId :: RouteDuplex' String -> RouteDuplex' TuneId
tuneId = as tuneIdToString tuneIdFromString

commentId :: RouteDuplex' String -> RouteDuplex' CommentId
commentId = as commentIdToString commentIdFromString

genre :: RouteDuplex' String -> RouteDuplex' Genre
genre = as genreToString genreFromString

data Route
  = Home
  | Genre
  | Login
  | Register
  | Upload
  | AdvancedSearch
  | UserList PageParams
  | Tune Genre TuneId
  | TuneList SearchParams
  | Comments Genre TuneId
  | Comment Genre TuneId String CommentId
  | Metronome
  | Tutorial
  | Editor  { initialAbc :: Maybe String }
  | About
  | Credits
  | ContactUs
  | Help


derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Login": "login" / noArgs
  , "Genre": "genre" / noArgs
  , "Register": "register" / noArgs
  , "Upload": "upload" / noArgs
  , "AdvancedSearch": "advancedsearch" / noArgs
  , "Tune": "genre" / (genre segment)  / "tune" / (tuneId segment)
  , "UserList": "users" ?
       { page: int }
  , "TuneList":  "tunelist" ?
       { key : optional <<< string
       , rhythm : optional <<< string
       , title : optional <<< string
       , source : optional <<< string
       , origin : optional <<< string
       , composer : optional <<< string
       , transcriber : optional <<< string
       , abc : optional <<< string
       , page: int
       , sort : string }
  , "Comments": "genre" / (genre segment)  / "tune" / (tuneId segment) / "comments"
  , "Comment": "genre" / (genre segment)  / "tune" / (tuneId segment) / "comment" / segment / (commentId segment)
  , "Metronome" : "metronome" / noArgs
  , "Tutorial" : "tutorial" / noArgs
  , "Editor" : "editor" ? { initialAbc: (optional <<< string) }
  , "About" : "about" / noArgs
  , "Credits" : "credits" / noArgs
  , "ContactUs" : "contact" / noArgs
  , "Help" : "help" / noArgs
  }
