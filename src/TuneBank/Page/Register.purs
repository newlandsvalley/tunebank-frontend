module TuneBank.Page.Register where

import Affjax.RequestBody (RequestBody(..))
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)
import Data.String (contains, length)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Validation.Semigroup
import Prelude (Unit, Void, ($), (<<<), (<>), (<), (/=), (<$>), (<*>), bind, discard, pure, unit)
import TuneBank.Data.Types (Validated)
import TuneBank.Data.Session (Session)
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Route (Route(..))



-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type Submission =
  { name :: String
  , email :: String
  , password :: String
  }

type State =
  { submission :: Submission
  , confirmationPassword :: String
  , userRegisterResult :: Either String String
  , errorText :: String
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


component :: ∀ i o m. MonadAff m => H.Component HH.HTML Query i o m
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
    let
      submission =
        { name : ""
        , email : ""
        , password : ""
        }
    in
      { submission
      , confirmationPassword : ""
      , userRegisterResult : Left "not registered"
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
      [ HH.text "register" ]

  renderRegisterError ::  State -> H.ComponentHTML Action ChildSlots m
  renderRegisterError state =
    HH.div_
      [
        HH.text state.errorText
      ]

    {-
    let
      errorText = either (\x -> ("registration failed " <> x)) (\_ -> "registration OK") state.userRegisterResult
    in
    -}

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
      H.modify_ (\st -> st { confirmationPassword = password } )
    RegisterUser -> do
      state <- H.get
      let
        validated = validate state.submission state.confirmationPassword
        newState = unV
                    (\errs -> state { errorText = foldl (<>) "" errs})
                    (\submission -> state {submission = submission, errorText = ""} )
                    validated
      _ <- H.put newState
      pure unit


-- validation
validate :: Submission -> String -> Validated Submission
validate submission confirmationPassword =
  { name : _, email : _, password : _ }
  <$> validateName submission.name
  <*> validateEmail submission.email
  <*> validatePassword submission.password confirmationPassword

validateName :: String -> Validated String
validateName name =
  if (0 < length name) then
    pure name
  else
    invalid $ pure ("Invalid name. ")

validatePassword :: String -> String -> Validated String
validatePassword password confirmationPassword =
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
