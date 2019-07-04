module TuneBank.Page.Tune where

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Control.Monad.Reader (class MonadAsk, asks)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<>), bind, pure, show, unit)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL(..), TuneId(..))
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre)


currentUser = Nothing

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , tuneURI :: String
  , tuneId :: TuneId
  , baseURL :: BaseURL
  }

type Input =
  { tuneURI :: String
  , tuneId :: TuneId
  }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Initialize

component
   :: ∀ o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
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
    { genre : Scandi
    , tuneURI : input.tuneURI
    , tuneId : input.tuneId
    , baseURL : BaseURL ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    let
      (TuneId {title, tuneType}) = state.tuneId
    in
      HH.div_
        [ header Nothing Home
        , HH.h1
           [HP.class_ (H.ClassName "center") ]
           [HH.text ("Tune " <> title) ]
        , renderTuneScore state title
        , footer
        ]

  renderTuneScore :: State -> String -> H.ComponentHTML Action ChildSlots m
  renderTuneScore state title =
    let
      imageURI = (show state.baseURL) <> "/genre/" <> (asUriComponent state.genre) <>
                 "/tune/" <> state.tuneURI <> "/png"
    in
      HH.div_
        [HH.img
          [ HP.src imageURI
          , HP.alt title
          ]
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      genre <- getCurrentGenre
      baseURL <- getBaseURL
      H.modify_ (\state -> state { genre = genre, baseURL = baseURL } )
