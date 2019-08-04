module TuneBank.Page.Register where

import Affjax.RequestBody (RequestBody(..))
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<<<), (<>), bind, discard, pure, unit)
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
    let
      errorText = either (\x -> ("registration failed " <> x)) (\_ -> "registration OK") state.userRegisterResult
    in
      HH.div_
        [
          HH.text errorText
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
      H.modify_ (\st -> st { confirmationPassword = password } )
    RegisterUser -> do
      pure unit
