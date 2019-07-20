module TuneBank.Api.Request where

import Prelude
import Affjax (Request, printResponseFormatError, request)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.ResponseFormat as RF
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Array (fromFoldable, head, filter)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap, lmap, rmap)
import Data.HTTP.Method (Method(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, getField, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Routing.Duplex (print)
import TuneBank.Navigation.Endpoint (PageParams, Endpoint(..), endpointCodec)
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Data.TuneId (TuneId())
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Api.Codec.TunesPage (TunesPage, decodeTunesPage)
import TuneBank.Api.Codec.UsersPage (UsersPage, decodeUsersPage)
import TuneBank.Api.Codec.Tune (TuneMetadata, fixJson, decodeTune)
import TuneBank.Api.Codec.CommentArray (CommentArray, decodeComments)
import TuneBank.Api.Codec.Pagination (Pagination, defaultPagination, decodePagination)
import TuneBank.Authorization.BasicAuth (authorizationHeader)
import TuneBank.BugFix.Backend (fixSearchParams)

import Debug.Trace (spy, traceM)

defaultJsonGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> Request Json
defaultJsonGetRequest (BaseURL baseUrl) mCredentials endpoint =
  { method: Left GET
  , url: baseUrl <> (fixSearchParams endpoint $ print endpointCodec endpoint)
  , headers: [  Accept (MediaType "application/json; charset=UTF-8")
             ] <>
               (fromFoldable $ authorizationHeader mCredentials)
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }

{-
defaultAbcGetRequest :: BaseURL -> Endpoint -> Request String
defaultAbcGetRequest (BaseURL baseUrl) endpoint =
  { method: Left GET
  , url: baseUrl <> print endpointCodec endpoint <> "/abc"
  , headers: [ Accept (MediaType "text/vnd.abc")]
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , withCredentials: true
  , responseFormat: RF.string
  }
-}

defaultStringGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> MediaType -> Request String
defaultStringGetRequest (BaseURL baseUrl) mCredentials endpoint mediaType =
  { method: Left GET
  , url: baseUrl <> print endpointCodec endpoint
  , headers: [ Accept mediaType] <>
              (fromFoldable $ authorizationHeader mCredentials)
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.string
  }

-- | only needed against pre v1.2.0 musicrest which exhibits JSON errors
defaultJsonAsStrGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> Request String
defaultJsonAsStrGetRequest (BaseURL baseUrl) mCredentials endpoint =
  let
    foo = spy "endpoint" $ fixSearchParams endpoint $ print endpointCodec endpoint
  in
    { method: Left GET
    , url: baseUrl <> (fixSearchParams endpoint $ print endpointCodec endpoint)
    , headers: [Accept applicationJSON ]<>
                (fromFoldable $ authorizationHeader mCredentials)
    , content: Nothing
    , username: map _.user mCredentials
    , password: map _.pass mCredentials
    , withCredentials: false
    , responseFormat: RF.string
    }

-- | this gives a bad JSON error because it really is bad!
requestTune :: forall m. MonadAff m => BaseURL -> String -> TuneId -> m (Either String TuneMetadata)
requestTune baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultJsonGetRequest baseUrl Nothing (Tune genre tuneId)
  let
    tune = (lmap printResponseFormatError res.body)
      >>= decodeTune
  pure $ tune

-- | only needed against pre v1.2.0 musicrest which exhibits JSON errors
requestCleanTune :: forall m. MonadAff m => BaseURL -> String -> TuneId -> m (Either String TuneMetadata)
requestCleanTune baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultJsonAsStrGetRequest baseUrl Nothing (Tune genre tuneId)
  let
    tune = (bimap printResponseFormatError fixJson res.body)
      >>= (\s -> jsonParser $ fixJson s)
      >>= decodeJson
      >>= decodeTune
  pure $ tune

requestTuneStr :: forall m. MonadAff m => BaseURL -> String -> TuneId -> m (Either String String)
requestTuneStr baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultJsonAsStrGetRequest baseUrl Nothing (Tune genre tuneId)
  pure $ lmap printResponseFormatError res.body

requestTuneAbc :: forall m. MonadAff m => BaseURL -> String -> TuneId -> m (Either String String)
requestTuneAbc baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultStringGetRequest baseUrl Nothing (Tune genre tuneId)  (MediaType "text/vnd.abc")
  pure $ lmap printResponseFormatError res.body

requestTuneSearch :: forall m. MonadAff m => BaseURL -> String -> SearchParams -> m (Either String (Tuple TunesPage Pagination))
requestTuneSearch baseUrl genre searchParams = do
  res <- H.liftAff $ request $ defaultJsonGetRequest baseUrl Nothing (Search genre searchParams)
  let
    pagination = getPagination res.headers
    tunesPage = (lmap printResponseFormatError res.body)
      >>= decodeTunesPage
    tuple = rmap (\page -> Tuple page pagination) tunesPage
  pure $ tuple

requestUsers :: forall m. MonadAff m => BaseURL -> Maybe Credentials-> PageParams -> m (Either String (Tuple UsersPage Pagination))
requestUsers baseUrl mCredentials pageParams = do
  res <- H.liftAff $ request $ defaultJsonGetRequest baseUrl mCredentials (Users pageParams)
  let
    pagination = getPagination res.headers
    usersPage = (lmap printResponseFormatError res.body)
      >>= decodeUsersPage
    tuple = rmap (\page -> Tuple page pagination) usersPage
  pure tuple

checkUser :: forall m. MonadAff m => BaseURL -> Credentials-> m (Either String String)
checkUser baseUrl credentials = do
  res <- H.liftAff $ request $ defaultStringGetRequest baseUrl (Just credentials) UserCheck (MediaType "text/plain; charset=UTF-8")
  let
    response = (lmap printResponseFormatError res.body)
  pure $ response


requestComments :: forall m. MonadAff m => BaseURL -> String -> TuneId -> m (Either String CommentArray)
requestComments baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultJsonGetRequest baseUrl Nothing (Comments genre tuneId)
  let
    comments = (lmap printResponseFormatError res.body)
      >>= decodeComments
  pure $ comments




{-
requestUsersStr :: forall m. MonadAff m => BaseURL -> Maybe Credentials -> PageParams -> m (Either String String)
requestUsersStr baseUrl mCredentials pageParams = do
  res <- H.liftAff $ request $ defaultJsonAsStrGetRequest baseUrl mCredentials (Users pageParams)
  let
    usersPage = (lmap printResponseFormatError res.body)
  pure $ usersPage
-}

requestTuneSearchStr :: forall m. MonadAff m => BaseURL -> String -> SearchParams -> m (Either String String)
requestTuneSearchStr baseUrl genre searchParams = do
  res <- H.liftAff $ request $ defaultJsonAsStrGetRequest baseUrl Nothing (Search genre searchParams)
  let
    tunesPage = (lmap printResponseFormatError res.body)
  pure $ tunesPage

requestCommentsStr :: forall m. MonadAff m => BaseURL -> String -> TuneId -> m (Either String String)
requestCommentsStr baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultJsonAsStrGetRequest baseUrl Nothing (Comments genre tuneId)
  let
    commentsPage = (lmap printResponseFormatError res.body)
  pure $ commentsPage

getPagination :: Array ResponseHeader-> Pagination
getPagination headers =
  fromMaybe defaultPagination $ map decodePagination $ paginationResponse headers

paginationResponse :: Array ResponseHeader -> Maybe String
paginationResponse headers =
  map value $ head $ filter (\h -> name h == "musicrest-pagination") headers
