module TuneBank.Page.Tune where

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<>), pure, unit)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (TuneId(..))
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.Navigation.Route (Route(..))
import Web.DOM.ParentNode (QuerySelector(..))


currentUser = Nothing

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { dummy :: Int
  , tuneId :: TuneId
  }

type Input =
  { genre :: Genre
  , tuneId :: TuneId
  }


type Query = (Const Void)

type ChildSlots = ()

data Action
  = Dummy

component :: ∀ o m. MonadAff m => H.Component HH.HTML Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Dummy
        , finalize = Nothing
        }
    }
  where

  initialState :: Input -> State
  initialState input =
    { dummy : 0
    , tuneId : input.tuneId
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
        , footer
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Dummy -> do
      pure unit
