module Test.Main where

import Prelude
import Control.Monad.Free (Free)
import Data.Array (length) as A
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Bifunctor (rmap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Global.Unsafe (unsafeDecodeURIComponent, unsafeEncodeURIComponent)
import Test.Unit (Test, TestF, suite, test, failure, success)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Routing.Duplex (print)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Data.TuneId (TuneId(..))
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Credentials (Role(..), Credentials)
import TuneBank.Api.Request (requestTune, requestTuneAbc, requestCleanTune, requestTuneStr,
       requestTuneSearch, requestTuneSearchStr, checkUser, requestUsers, requestComments,
       requestCommentsStr)
import TuneBank.Navigation.Endpoint (PageParams)
import TuneBank.Navigation.Route (Route(..), routeCodec)
import TuneBank.Navigation.SearchParams (SearchParams, defaultSearchParams, parseParams)


import Debug.Trace (spy, trace)

assertRight :: forall a b. Either a b -> Test
assertRight either =
  case either of
    Left _ -> failure ("Right expected")
    Right _ -> success

baseURL :: BaseURL
-- production server
-- baseURL = BaseURL "http://www.tradtunedb.org.uk:8080/musicrest"
baseURL = BaseURL "http://192.168.0.113:8080/musicrest"

sampleTune :: TuneId
sampleTune =
  TuneId $ { title : "antara", tuneType: "reel" }

sampleCommentedTune :: TuneId
sampleCommentedTune =
  -- production server
  -- TuneId $ { title : "andet+brudestykke", tuneType: "marsch" }
  TuneId $ { title : "vals+a+lulu", tuneType: "waltz" }

simpleSearch :: SearchParams
simpleSearch =
  defaultSearchParams

complexSearch :: SearchParams
complexSearch =
  simpleSearch { key = Just "Dmaj", rhythm = Just "polska"}

page1 :: PageParams
page1 =
  { page: 1 }

adminUser :: Credentials
adminUser =
  { user : "administrator"
  , pass : "h0rsf0rth"
  , role : Administrator
  }
  {- production server
    { user : "administrator"
    , pass : "b*"
    , role : Administrator
    }
  -}


unknownUser :: Credentials
unknownUser =
  { user : "unknown"
  , pass : "xyz123"
  , role : NormalUser
  }

{-}
samplePredicateString :: String
samplePredicateString =
  "T=antara-reel&abc= F G A"

samplePredicate :: SearchPredicate
samplePredicate =
  [ { key: "R", value: "polska" }
  , { key: "K", value: "Dmaj" }
  ]
-}

main :: Effect Unit
main = runTest do
  apiSuite
  codecSuite
  routesSuite

apiSuite :: Free TestF Unit
apiSuite =
  suite "Endpoint API" do
    test "get tune" do
      resource <- requestCleanTune baseURL Irish sampleTune
      assertRight resource
    {-
    test "get tune" do
      resource <- requestTuneStr baseURL "irish" sampleTune
      Assert.equal (Left "error") resource
    -}
    test "get tune ABC" do
      resource <- requestTuneAbc baseURL Irish sampleTune
      assertRight resource
    test "simple search" do
      response <- requestTuneSearch baseURL "irish" simpleSearch
      Assert.equal (Right "15") $ rmap (\(Tuple tunes pagination) -> tunes.pageNum.size) response
      Assert.equal (Right 1) $  rmap (\(Tuple tunes pagination) -> pagination.page) response
      Assert.equal (Right 5) $  rmap (\(Tuple tunes pagination) -> pagination.maxPages) response
      -- production server
      -- Assert.equal (Right 6) $  rmap (\(Tuple tunes pagination) -> pagination.maxPages) response
    test "complex search" do
      response <- requestTuneSearch baseURL "scandi" complexSearch
      Assert.equal (Right "15") $ rmap (\(Tuple tunes pagination) -> tunes.pageNum.size) response
      Assert.equal (Right 3) $ rmap (\(Tuple tunes pagination) -> pagination.maxPages) response
    test "get users" do
      response  <- requestUsers baseURL (Just adminUser) page1
      Assert.equal (Right "15") $  rmap (\(Tuple users pagination) -> users.pageNum.size) response
      Assert.equal (Right 1) $  rmap (\(Tuple users pagination) -> pagination.page) response
    test "check good user" do
      userCheck <- checkUser baseURL adminUser
      Assert.equal (Right "user is valid") userCheck
    test "check bad user" do
      userCheck <- checkUser baseURL unknownUser
      Assert.equal (Right "The supplied authentication is invalid") userCheck
   {-
    test "get tune comments" do
      resource <- requestCommentsStr baseURL "scandi" sampleCommentedTune
      Assert.equal (Left "error") resource
    -}
    test "get tune comments" do
      comments <- requestComments baseURL Scandi sampleCommentedTune
      Assert.equal (Right 1) $ rmap A.length comments
    test "get tune empty comments" do
      comments <- requestComments baseURL Scandi sampleTune
      Assert.equal (Right 0) $ rmap A.length comments

    test "complex search str" do
      response <- requestTuneSearchStr baseURL "scandi" complexSearch
      foo <- case response of
          Right tunes ->
            pure $ spy "tunes list" tunes
          Left _ ->
            pure ""
      -- Assert.equal (Left "error") $ response
      assertRight response

codecSuite :: Free TestF Unit
codecSuite =
  suite "Codecs" do
    test "tune name" do
      let
        sampleTuneName = "andet brudestykke"
      Assert.equal sampleTuneName
        (unsafeDecodeURIComponent $ unsafeEncodeURIComponent sampleTuneName)
    test "parse search params" do
      let
        expected = defaultSearchParams { rhythm = Just "reel", key = Just "BMin" }
      Assert.equal (Right expected) (parseParams "rhythm=reel&key=BMin")

routesSuite :: Free TestF Unit
routesSuite =
  suite "Routes" do
    test "Home" do
      Assert.equal "/" (print routeCodec Home)
    test "Login" do
      Assert.equal "/login" (print routeCodec Login)
    test "TuneList" do
      Assert.equal "/tunelist?sort=alpha&page=1" (print routeCodec $
        TuneList
             { key : Nothing
             , rhythm : Nothing
             , title : Nothing
             , source : Nothing
             , origin : Nothing
             , composer : Nothing
             , transcriber : Nothing
             , abc : Nothing
             , page: 1
             , sort : "alpha" })
    test "UserList" do
      Assert.equal "/users?page=1" (print routeCodec $ UserList { page : 1 })

{-}
predicateSuite :: Free TestF Unit
predicateSuite =
  suite "Search predicate" do
    test "round trip string" do
      Assert.equal samplePredicateString $ (printPredicate <<< parsePredicateHushed) samplePredicateString
    test "round trip predicate" do
      Assert.equal samplePredicate $ (parsePredicateHushed <<< printPredicate) samplePredicate



parsePredicateHushed :: String -> SearchPredicate
parsePredicateHushed s =
  fromMaybe [] $ hush $ parsePredicate s
-}
