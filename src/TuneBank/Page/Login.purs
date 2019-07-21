module TuneBank.Page.Login where

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either, isRight)
import Data.String.CodePoints (length)
import Control.Monad.Reader (class MonadAsk, asks)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<<<), (>), bind, identity, pure, unit)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Session (Session)
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Data.Credentials (Credentials, blankCredentials)
import TuneBank.Page.Utils.Environment (getBaseURL)
import TuneBank.Api.Request (checkUser)
import TuneBank.Page.Utils.Environment (getUser)



-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { credentials :: Credentials
  , currentUser :: Maybe Credentials
  , userCheckResult :: Either String String
  }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Initialize
  | HandleUserName String
  | HandlePassword String
  | LoginUser

component :: ∀ i o m r
  . MonadAff m
  => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
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
    { credentials : blankCredentials
    , currentUser : Nothing
    , userCheckResult : Left ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ header state.currentUser Login
      , HH.form
        [ HP.id_ "loginform" ]
        [ HH.fieldset
            []
            [ HH.legend_ [HH.text "Login"]
            , renderUserName state
            , renderPassword state
            , renderLoginButton state
            ]
        , renderLoginError state
        ]
      , footer
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

  renderPassword :: State -> H.ComponentHTML Action ChildSlots m
  renderPassword state =
    HH.div
      [ css "textinput-div" ]
      [ HH.label
        [ css "textinput-label" ]
        [ HH.text "password:" ]
      , HH.input
          [ css "textinput"
          , HE.onValueInput  (Just <<< HandlePassword)
          , HP.value ""
          , HP.type_ HP.InputPassword
          ]
      ]

  renderLoginButton :: State -> H.ComponentHTML Action ChildSlots m
  renderLoginButton state =
      HH.button
        [ HE.onClick \_ -> Just LoginUser
        , css "hoverable"
        , HP.enabled true
        ]
        [ HH.text "login" ]

  renderLoginError ::  State -> H.ComponentHTML Action ChildSlots m
  renderLoginError state =
    let
      errorText = either identity (\_ -> "login OK") state.userCheckResult
    in
      HH.div_
        [
          HH.text errorText
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      mUser <- getUser
      _ <- H.modify (\state -> state { currentUser = mUser } )
      pure unit
    HandleUserName name -> do
      if (length name > 0)
        then do
          state <- H.get
          let
            credentials = state.credentials { user = name }
          H.modify_ (\st -> st { credentials = credentials } )
        else pure unit
    HandlePassword pass -> do
      if (length pass > 0)
        then do
          state <- H.get
          let
            credentials = state.credentials { pass = pass }
          H.modify_ (\st -> st { credentials = credentials } )
        else pure unit
    LoginUser -> do
      state <- H.get
      baseURL <- getBaseURL
      userCheckResult <- checkUser baseURL state.credentials
      _ <- H.modify_ (\st -> st { userCheckResult = userCheckResult } )
      if (isRight userCheckResult)
        then do
          session <- asks _.session
          _ <- H.liftEffect $ Ref.write (Just state.credentials) session.user
          navigate Home
        else  pure unit
