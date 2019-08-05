module TuneBank.Page.Register where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String (contains, length)
import Data.String.Common (null)
import Data.String.Pattern (Pattern(..))
import Data.Validation.Semigroup (invalid, unV)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TuneBank.Api.Codec.Register (Submission, defaultSubmission)
import TuneBank.Api.Codec.Utils (containsDigit)
import TuneBank.Api.Request (postNewUser)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (Validated, BaseURL)
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Page.Utils.Environment (getBaseURL)

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { submission :: Submission
  , userRegisterResult :: Either String String  -- result from server
  , errorText :: String                         -- validation errors
  }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Initialize
  | HandleUserName String
  | HandleEmail String
  | HandlePassword String
  | HandlePasswordConfirmation String
  | RegisterUser

component
   :: ∀ i o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL  | r } m
   => Navigate m
   => H.Component HH.HTML Query i o m
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

  initialState :: i -> State
  initialState _ =
    { submission : defaultSubmission
    , userRegisterResult : Left ""
    , errorText : ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.form
      [ HP.id_ "registrationform" ]
      [ HH.fieldset
        []
        [ HH.legend_ [HH.text "Register"]
        , renderUserName state
        , renderEmail state
        , renderPassword false state
        , renderPassword true state
        , renderRegisterButton
        ]
      , renderRegisterError state
      ]

  renderUserName :: State -> H.ComponentHTML Action ChildSlots m
  renderUserName state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
        [ css "textinput-label" ]
        [ HH.text "name:" ]
      , HH.input
          [ css "textinput"
          , HE.onValueInput  (Just <<< HandleUserName)
          , HP.value ""
          , HP.type_ HP.InputText
          ]
      ]


  renderEmail :: State -> H.ComponentHTML Action ChildSlots m
  renderEmail state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
        [ css "textinput-label" ]
        [ HH.text "email:" ]
      , HH.input
          [ css "textinput"
          , HE.onValueInput  (Just <<< HandleEmail)
          , HP.value ""
          , HP.type_ HP.InputText
          ]
      ]

  renderPassword :: Boolean -> State -> H.ComponentHTML Action ChildSlots m
  renderPassword isConfirmation state =
    let
      action =
        if isConfirmation then
          HandlePasswordConfirmation
        else
          HandlePassword
      label =
        if isConfirmation then
          "confirm password:"
        else
          "password:"
    in
      HH.div
        [ css "textinput-div" ]
        [ HH.label
          [ css "textinput-label" ]
          [ HH.text label ]
        , HH.input
            [ css "textinput"
            , HE.onValueInput  (Just <<< action)
            , HP.value ""
            , HP.type_ HP.InputPassword
            ]
        ]

  renderRegisterButton :: H.ComponentHTML Action ChildSlots m
  renderRegisterButton =
    HH.button
      [ HE.onClick \_ -> Just RegisterUser
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "register user" ]

  renderRegisterError ::  State -> H.ComponentHTML Action ChildSlots m
  renderRegisterError state =
    let
      registrationOK :: String -> String
      registrationOK msg =
          msg <>
            "-- the last part of the registration process is to reply to an" <>
            " email confirmation message that has been sent to you."
      registrationText = either identity registrationOK state.userRegisterResult
    in
      HH.div_
        [
          HH.text state.errorText
        , HH.text registrationText
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit
    HandleUserName name -> do
      state <- H.get
      let
        newSubmission = state.submission { name = name }
      H.modify_ (\st -> st { submission = newSubmission } )
    HandleEmail email -> do
      state <- H.get
      let
        newSubmission = state.submission { email = email }
      H.modify_ (\st -> st { submission = newSubmission } )
    HandlePassword password -> do
      state <- H.get
      let
        newSubmission = state.submission { password = password }
      H.modify_ (\st -> st { submission = newSubmission } )
    HandlePasswordConfirmation password -> do
      state <- H.get
      let
        newSubmission = state.submission { password2 = password }
      H.modify_ (\st -> st { submission = newSubmission } )
    RegisterUser -> do
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
      pure unit


-- validation
validate :: Submission -> Validated Submission
validate submission  =
  { name : _
  , email : _
  , password : _
  , password2 : submission.password2
  , refererUrl : submission.refererUrl }
  <$> validateName submission.name
  <*> validateEmail submission.email
  <*> validatePassword submission.password submission.password2

validateName :: String -> Validated String
validateName name =
  if (0 < length name) then
    pure name
  else
    invalid $ pure ("Invalid name. ")

validatePassword :: String -> String -> Validated String
validatePassword password confirmationPassword =
  if (containsDigit password)  && length password >= 7
    then
      comparePasswords password confirmationPassword
    else
      invalid $ pure ("Passwords should be at least 7 characters and contain at least one digit. ")

comparePasswords :: String -> String -> Validated String
comparePasswords password confirmationPassword =
  if (password /= confirmationPassword ) then
    invalid $ pure ("Passwords don't match. ")
  else
    pure password

validateEmail :: String -> Validated String
validateEmail email =
  if (contains (Pattern "@") email) then
    pure email
  else
    invalid $ pure ("Invalid email. ")
