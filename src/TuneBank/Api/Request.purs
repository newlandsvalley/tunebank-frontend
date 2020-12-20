module TuneBank.Api.Request where

import Prelude

import Affjax (Request, printError, request)
import Affjax.RequestBody (formURLEncoded)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (fromFoldable)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded, fromArray) as FUE
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect.Aff (try)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Routing.Duplex (print)
import TuneBank.Api.Codec.Comments (Comment, Comments, decodeComment, decodeComments)
import TuneBank.Api.Codec.Comments (Comment, encodeFormData) as Comments
import TuneBank.Api.Codec.Register (Submission, encodeFormData) as Register
import TuneBank.Api.Codec.Tune (TuneMetadata, decodeTune)
import TuneBank.Api.Codec.TunesPage (TunesPage, decodeTunesPage)
import TuneBank.Api.Codec.UsersPage (UsersPage, decodeUsersPage)
import TuneBank.Api.Codec.Utils (encodeURIComponent)
import TuneBank.Authorization.BasicAuth (authorizationHeader)
import TuneBank.BugFix.Backend (fixSearchParams)
import TuneBank.Data.CommentId (CommentId, CommentKey)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import TuneBank.Data.TuneId (TuneId)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Navigation.Endpoint (PageParams, Endpoint(..), endpointCodec)
import TuneBank.Navigation.SearchParams (SearchParams)

defaultJsonGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> Request Json
defaultJsonGetRequest (BaseURL baseUrl) mCredentials endpoint =
  let
    foo = spy "endpoint" $ fixSearchParams endpoint $ print endpointCodec endpoint
  in
    { method: Left GET
    , url: baseUrl <> (fixSearchParams endpoint $ print endpointCodec endpoint)
    , headers: [  Accept (MediaType "application/json; charset=UTF-8")
               ] <>
                 (fromFoldable $ authorizationHeader mCredentials)
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , timeout: Nothing
    , withCredentials: false
    , responseFormat: RF.json
    }

defaultStringGetRequest :: BaseURL -> Maybe Credentials -> Endpoint -> MediaType -> Request String
defaultStringGetRequest (BaseURL baseUrl) mCredentials endpoint mediaType =
  { method: Left GET
  , url: baseUrl <> print endpointCodec endpoint
  , headers: [ Accept mediaType] <>
              (fromFoldable $ authorizationHeader mCredentials)
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
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
    , timeout: Nothing
    , withCredentials: false
    , responseFormat: RF.string
    }

defaultPostRequest :: BaseURL -> Maybe Credentials -> FUE.FormURLEncoded -> Endpoint -> Request String
defaultPostRequest (BaseURL baseUrl) mCredentials fue endpoint  =
  { method: Left POST
  , url: baseUrl <> print endpointCodec endpoint
  , headers: (fromFoldable $ authorizationHeader mCredentials)
  , content: Just $ formURLEncoded fue
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.string
  }

defaultDeleteRequest :: BaseURL -> Maybe Credentials -> Endpoint -> Request String
defaultDeleteRequest (BaseURL baseUrl) mCredentials endpoint  =
  { method: Left DELETE
  , url: baseUrl <> print endpointCodec endpoint
  , headers: (fromFoldable $ authorizationHeader mCredentials)
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.string
  }


-- | this gives a bad JSON error because it really is bad!
requestTune :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String TuneMetadata)
requestTune baseUrl genre tuneId = do
  res <- H.liftAff $ tryRequest $ defaultJsonGetRequest baseUrl Nothing (Tune genre tuneId)
  case res of
    Left err ->
      pure $ Left $ show err
    Right json -> do
      let
        -- tune :: Either String Tune
        tune = lmap printJsonDecodeError $ decodeTune json
      pure $ tune


requestTuneStr :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String String)
requestTuneStr baseUrl genre tuneId = do
  res <- H.liftAff $ tryRequest $ defaultJsonAsStrGetRequest baseUrl Nothing (Tune genre tuneId)
  case res of
    Left err ->
      pure $ Left $ show err
    Right body -> do
      pure $ Right body

requestTuneAbc :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String String)
requestTuneAbc baseUrl genre tuneId = do
  res <- H.liftAff $ request $ defaultStringGetRequest baseUrl Nothing (Tune genre tuneId)  (MediaType "text/vnd.abc")
  pure $ bimap printError _.body res

requestTuneSearch :: forall m. MonadAff m => BaseURL -> String -> SearchParams -> m (Either String TunesPage)
requestTuneSearch baseUrl genre searchParams = do
  res <- H.liftAff $ tryRequest $ defaultJsonGetRequest baseUrl Nothing (Search genre searchParams)
  case res of
    Left err ->
      pure $ Left $ show err
    Right json -> do
      let
        -- tunesPage :: Either String TunesPage
        tunesPage = lmap printJsonDecodeError $ decodeTunesPage json
      pure $ tunesPage


requestUsers :: forall m. MonadAff m => BaseURL -> Maybe Credentials-> PageParams -> m (Either String UsersPage)
requestUsers baseUrl mCredentials pageParams = do
  res <- H.liftAff $ tryRequest $ defaultJsonGetRequest baseUrl mCredentials (Users pageParams)
  case res of
    Left err ->
      pure $ Left $ show err
    Right json -> do
      let
        -- usersPage :: Either String UsersPage
        usersPage = lmap printJsonDecodeError $ decodeUsersPage json
      pure $ usersPage

checkUser :: forall m. MonadAff m => BaseURL -> Credentials-> m (Either String String)
checkUser baseUrl credentials = do
  res <- H.liftAff $ tryRequest $ defaultStringGetRequest baseUrl (Just credentials) UserCheck (MediaType "text/plain; charset=UTF-8")
  case res of
    Left err -> do
      let
        foo = spy "check user Left response" err
      pure $ Left $ show err
    Right body -> do
      let
        -- response = (lmap printError res.body)
        foo = spy "check user Right response" body
      pure $ Right body

-- | check the MusicRest service is up by attempting to get a welcome message
-- | we never need to bother to decode the JSON
checkService :: forall m. MonadAff m => BaseURL -> m (Either String Json)
checkService baseUrl =  H.liftAff do
  res1 <- tryRequest $ defaultJsonGetRequest baseUrl Nothing Root
  pure res1

requestComments :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String Comments)
requestComments baseUrl genre tuneId = do
  res1 <- H.liftAff $ tryRequest $ defaultJsonGetRequest baseUrl Nothing (Comments genre tuneId)
  case res1 of
    Left err ->
      pure $ Left $ show err
    Right json -> do
      let
        comments :: Either String Comments
        comments = lmap printJsonDecodeError $ decodeComments json
      pure $ comments

requestComment :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> CommentKey -> Credentials -> m (Either String Comment)
requestComment baseUrl genre tuneId key credentials =
  H.liftAff do
    let
      encodedUser = encodeURIComponent key.user
    res <- tryRequest $ defaultJsonGetRequest baseUrl (Just credentials) (Comment genre tuneId encodedUser key.commentId)
    case res of
      Left err ->
        pure $ Left $ show err
      Right json -> do
        let
          comment :: Either String Comment
          comment = lmap printJsonDecodeError $ decodeComment json
        pure $ comment


requestTuneSearchStr :: forall m. MonadAff m => BaseURL -> String -> SearchParams -> m (Either String String)
requestTuneSearchStr baseUrl genre searchParams = do
  res <- H.liftAff $ tryRequest $ defaultJsonAsStrGetRequest baseUrl Nothing (Search genre searchParams)
  case res of
    Left err ->
      pure $ Left $ show err
    Right body -> do
      pure $ Right body

requestCommentsStr :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> m (Either String String)
requestCommentsStr baseUrl genre tuneId = do
  res <- H.liftAff $ tryRequest $ defaultJsonAsStrGetRequest baseUrl Nothing (Comments genre tuneId)
  case res of
    Left err ->
      pure $ Left $ show err
    Right body -> do
      pure $ Right body

-- | POST

postTune :: forall m. MonadAff m => String -> BaseURL -> Genre -> Credentials -> m (Either String String)
postTune tuneAbc baseUrl genre credentials =
  H.liftAff do
    let
      formData = FUE.fromArray [ Tuple "abc"  (Just tuneAbc)]
    res <- tryRequest $ defaultPostRequest baseUrl (Just credentials) formData (NewTune genre)
    pure res

postNewUser :: forall m. MonadAff m => Register.Submission -> BaseURL -> m (Either String String)
postNewUser submission baseUrl =
  H.liftAff do
    let
      formData = Register.encodeFormData submission
    res <- tryRequest $ defaultPostRequest baseUrl Nothing formData Register
    pure res

postComment :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> Comments.Comment -> Credentials -> m (Either String String)
postComment baseUrl genre tuneId comment credentials =
  H.liftAff do
    let
      formData = Comments.encodeFormData comment
    res <- tryRequest $ defaultPostRequest baseUrl (Just credentials) formData (Comments genre tuneId)
    pure res

-- | DELETE
deleteTune :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> Credentials -> m (Either String String)
deleteTune baseUrl genre tuneId credentials =
  H.liftAff do
    res <- tryRequest $ defaultDeleteRequest baseUrl (Just credentials) (Tune genre tuneId)
    pure res


deleteComment :: forall m. MonadAff m => BaseURL -> Genre -> TuneId -> CommentId -> Credentials -> m (Either String String)
deleteComment baseUrl genre tuneId commentId credentials =
  H.liftAff do
    let
      encodedUser = encodeURIComponent credentials.user
    res <- tryRequest $ defaultDeleteRequest baseUrl (Just credentials) (Comment genre tuneId encodedUser commentId)
    pure res

-- | The default manner of attempting a request. All errors will be collected
-- | in Left String, irrespective of whether they emanate from bad HTTP responses
-- | or formatting errors.
tryRequest :: ∀ a m. MonadAff m => Request a -> m (Either String a)
tryRequest r = H.liftAff do
  res1 <- {-try $ -} request r
  case res1 of
    Left err1 ->
      pure $ Left $ printError err1
    Right response -> do
      pure $ Right response.body


{- These functions are no longer needed - we now ger pagination information
   directly from the JSON and not from the custom pagination header

getPagination :: Array ResponseHeader-> Pagination
getPagination headers =
  fromMaybe defaultPagination $ map decodePagination $ paginationResponse headers

paginationResponse :: Array ResponseHeader -> Maybe String
paginationResponse headers =
  map value $ head $ filter (\h -> name h == "musicrest-pagination") headers

  -}
