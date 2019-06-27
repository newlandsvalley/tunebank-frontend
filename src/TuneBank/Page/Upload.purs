module TuneBank.Page.Upload where

import Prelude (Unit, Void, ($), pure, unit)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import TuneBank.HTML.Header (header)
import TuneBank.HTML.Footer (footer)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)


-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { dummy :: Int }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Dummy

component :: ∀ i o m. MonadAff m => H.Component HH.HTML Query i o m
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

  initialState :: i -> State
  initialState _ =
    { dummy : 0 }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ header Nothing Upload
      , HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Upload" ]
      , footer
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Dummy -> do
      pure unit
