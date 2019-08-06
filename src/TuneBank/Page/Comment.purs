module TuneBank.Page.Comment where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TuneBank.Api.Codec.Comments (Submission, defaultSubmission)
import TuneBank.Api.Request (postComment)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..))
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Page.Utils.Environment (getBaseURL, getInstruments, getUser)

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , tuneId :: TuneId
  , baseURL :: BaseURL
  , submission :: Submission
  , submitCommentResult :: Either String String  -- result from server
  , errorText :: String                          -- validation errors
  }

type Query = (Const Void)

type Input =
  { genre :: Genre
  , tuneId :: TuneId
  }

type ChildSlots = ()

data Action
  = Initialize
  | HandleSubject String
  | HandleText String
  | SubmitComment

component
   :: ∀ o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL  | r } m
   => Navigate m
   => H.Component HH.HTML Query Input o m
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
    , submission : defaultSubmission
    , submitCommentResult : Left ""
    , errorText : ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
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
           [ HP.id_ "commentform" ]
           [ HH.fieldset
             []
             [ HH.legend_ [HH.text "Edit Comment"]
             , renderSubject state
             , renderText state
             , renderAdvisoryText state
             , renderSubmitButton state
             ]
           , renderRegisterError state
           ]
        ]


  renderAdvisoryText :: State -> H.ComponentHTML Action ChildSlots m
  renderAdvisoryText state =
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
          , HE.onValueInput  (Just <<< HandleSubject)
          , HP.value state.submission.subject
          , HP.type_ HP.InputText
          ]
      ]


  renderText :: State -> H.ComponentHTML Action ChildSlots m
  renderText state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
        [ css "textinput-label" ]
        [ HH.text "text:" ]
      , HH.textarea
         [ HP.rows 8
         , HP.cols 45
         , HP.autofocus true
         , HP.value state.submission.text
         , css "comment-edit"
         , HE.onValueInput (Just <<< HandleText)
         ]
      ]

  renderSubmitButton :: State -> H.ComponentHTML Action ChildSlots m
  renderSubmitButton state =
    let
      enabled =
        (length state.submission.subject > 0)  &&
        (length state.submission.text > 0)
    in
      HH.button
        [ HE.onClick \_ -> Just SubmitComment
        , css "hoverable"
        , HP.enabled enabled
        ]
        [ HH.text "submit comment" ]

  renderRegisterError ::  State -> H.ComponentHTML Action ChildSlots m
  renderRegisterError state =
    let
      submissionText = either identity identity state.submitCommentResult
    in
      HH.div_
        [
          HH.text state.errorText
        , HH.text submissionText
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      currentUser <- getUser
      baseURL <- getBaseURL
      H.modify_ (\st -> st
        { currentUser = currentUser
        , baseURL = baseURL
        } )
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
    SubmitComment -> do
      {-}
      state <- H.get
      baseURL <- getBaseURL
      -- reset any previous error text
      H.modify_ (\st -> st { userRegisterResult = Left "" } )
      let
        validated = validate state.submission
        newState = unV
                    (\errs -> state { errorText = foldl (<>) "" errs})
                    (\submission -> state {submission = submission
                                          , errorText = ""
                                          , userRegisterResult = Left ""} )
                    validated

      if (null newState.errorText)
        then do
          userRegisterResult <- postNewUser newState.submission baseURL
          _ <- H.put newState { userRegisterResult = userRegisterResult }
          pure unit
        else do
          _ <- H.put newState
          pure unit
      -}
      pure unit
