module TuneBank.Page.Comment where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (length, replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import TuneBank.Api.Codec.Comments (Comment, defaultComment)
import TuneBank.Api.Request (postComment, requestComment)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..))
import TuneBank.Data.CommentId (CommentKey, fromNow)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getUser)

-- | Edit a comment
-- | This is used both for editing existing comments and new ones

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , tuneId :: TuneId
  , baseURL :: BaseURL
  , key :: Maybe CommentKey
  , submission :: Comment
  , submitCommentResult :: Either String String  -- result from server
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type Input =
  { genre :: Genre
  , tuneId :: TuneId
  , key :: Maybe CommentKey
  }

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleSubject String
  | HandleText String
  | SubmitComment MouseEvent

component
   :: ∀ o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL  | r } m
   => Navigate m
   => H.Component Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Nothing
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { genre : input.genre
    , currentUser : Nothing
    , tuneId : input.tuneId
    , baseURL : BaseURL ""
    , key : input.key
    , submission : defaultComment
    , submitCommentResult : Left ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    if (isNothing state.currentUser) then
      HH.div_
       [ HH.form
         [ HP.id "commentform" ]
         [ HH.text "you must log in before submitting comments"]
       ]
    else
      renderForm state

  renderForm :: State -> H.ComponentHTML Action ChildSlots m
  renderForm state =
    let
      (TuneId { title, tuneType}) = state.tuneId
    in
      HH.div_
        [ HH.h2
           [ css "center" ]
           [ HH.text ("comment for " <> title) ]
        , HH.form
           [ HP.id "commentform" ]
           [ HH.fieldset
             []
             [ HH.legend_ [HH.text "Comment"]
             , renderSubject state
             , renderText state
             , renderAdvisoryText state
             , renderSubmitButton state
             ]
           , renderSubmissionError state
           ]
        ]


  renderAdvisoryText :: State -> H.ComponentHTML Action ChildSlots m
  renderAdvisoryText _state =
    let
      text1 =
        "Your comments can simply be text which may contain links to other sites" <>
        " with recordings or other information about the tune. If you find a YouTube " <>
        " video of the tune, the best technique is to follow "

      text2 =
         " to embed the video and then copy the code they provide into the text box." <>
         " Alternatively you can copy the watch URL of the video you're viewing from" <>
         " your web browser’s address bar and we'll attempt to embed the video into the comment"
    in
      HH.div_
        [ HH.p
          []
          [ HH.text text1
          , HH.a
            [ HP.href "https://support.google.com/youtube/answer/171780?hl=en-GB" ]
            [ HH.text "these instructions"]
          , HH.text text2
          ]
        ]


  renderSubject :: State -> H.ComponentHTML Action ChildSlots m
  renderSubject state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
        [ css "textinput-label" ]
        [ HH.text "subject:" ]
      , HH.input
          [ css "textinput"
          , HE.onValueInput  HandleSubject
          , HP.value state.submission.subject
          , HP.type_ HP.InputText
          ]
      ]

  -- we allow any text but embedded doube quotes are problematic
  renderText :: State -> H.ComponentHTML Action ChildSlots m
  renderText state =
    HH.div
      [ HP.id "comment-textinput-div" ]
      [ HH.label
        [ HP.id "comment-textinput-label" ]
        [ HH.text "text:" ]
      , HH.textarea
         [ HP.autofocus true
         , HP.value state.submission.text
         , css "comment-edit"
         , HE.onValueInput HandleText
         ]
      ]

  renderSubmitButton :: State -> H.ComponentHTML Action ChildSlots m
  renderSubmitButton state =
    let
      enabled =
        (length state.submission.subject > 0)  &&
        (length state.submission.text > 0)
      className =
        if enabled then "hoverable" else "unhoverable"
    in
      HH.button
        [ HE.onClick SubmitComment
        , css className
        , HP.enabled enabled
        ]
        [ HH.text "submit comment" ]

  renderSubmissionError ::  State -> H.ComponentHTML Action ChildSlots m
  renderSubmissionError state =
    let
      submissionText = either identity (const "") state.submitCommentResult
    in
      HH.div_
        [ HH.text submissionText ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      currentUser <- getUser
      baseURL <- getBaseURL
      let
        newState = state { currentUser = currentUser
                         , baseURL = baseURL
                         }
      case newState.currentUser of
        Just credentials -> do
          case newState.key of
            Nothing -> do
              H.put newState
              pure unit
            Just key -> do
              eComment <- H.liftAff $ requestComment baseURL state.genre state.tuneId key credentials
              case eComment of
                Right comment -> do
                  -- edit comment
                  H.put $ newState { submission = comment }
                  pure unit
                Left _ -> do
                  -- new comment
                  H.put newState
                  pure unit
        Nothing ->
          -- a user login is required before anything can happen in this page
          pure unit
    HandleSubject subject -> do
      state <- H.get
      let
        newSubmission = state.submission { subject = subject }
      H.modify_ (\st -> st { submission = newSubmission } )
    HandleText text -> do
      state <- H.get
      let
        newSubmission = state.submission { text = text }
      H.modify_ (\st -> st { submission = newSubmission } )
    SubmitComment event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      state <- H.get
      case state.currentUser of
        Nothing ->
          pure unit
        Just credentials -> do
          baseURL <- getBaseURL
          commentId <- H.liftEffect fromNow
          let
            submission =
              case state.key of
                Nothing ->
                  -- new comment
                  state.submission
                    { user = credentials.user
                    , commentId = commentId
                    , text = cleanCommentText state.submission.text
                    }
                Just _ ->
                   -- edit comment
                   state.submission
          submitCommentResult <- H.liftAff $ postComment baseURL state.genre state.tuneId submission credentials
          H.modify_ (\st -> st { submitCommentResult = submitCommentResult } )
          case submitCommentResult of
            Left _ ->
              pure unit
            Right _ ->
              -- go back to the tune page which should now show the comment
              navigate $ Tune state.genre state.tuneId

-- | replace any double quotes with single quotes
cleanCommentText :: String -> String
cleanCommentText =
  replaceAll (Pattern "\"") (Replacement "'")
