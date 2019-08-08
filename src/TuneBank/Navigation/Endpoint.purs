module TuneBank.Navigation.Endpoint
  (PageParams, Endpoint(..), endpointCodec)  where

-- | Endpoints - legitimate URLs at he musicrest backend


import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root, segment, as, int, optional, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import TuneBank.Data.TuneId (TuneId, tuneIdFromString, tuneIdToString)
import TuneBank.Data.CommentId (CommentId, commentIdFromString, commentIdToString)
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Data.Genre (Genre, genreFromString, genreToString)

type PageParams =
  { page :: Int }

tuneId :: RouteDuplex' String -> RouteDuplex' TuneId
tuneId = as tuneIdToString tuneIdFromString

commentId :: RouteDuplex' String -> RouteDuplex' CommentId
commentId = as commentIdToString commentIdFromString

genre :: RouteDuplex' String -> RouteDuplex' Genre
genre = as genreToString genreFromString


data Endpoint
  = Search String SearchParams
  | Users PageParams
  | UserCheck
  | Register
  | Tune Genre TuneId
  | NewTune Genre
  | Comments Genre TuneId
  | Comment Genre TuneId String CommentId

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
  , "Register": "user" / noArgs
  , "Tune": "genre" /  (genre segment) / "tune" / (tuneId segment)
  , "NewTune": "genre" / (genre segment) / "tune"
  , "Comments": "genre" / (genre segment) / "tune" / (tuneId segment) / "comments"
  , "Comment": "genre" / (genre segment) / "tune" / (tuneId segment) / "comment" / segment / (commentId segment)
  }
