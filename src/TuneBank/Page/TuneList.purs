module TuneBank.Page.TuneList where

import Prelude (Unit, Void, ($), bind, pure, unit)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..), hush)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk, asks)
import TuneBank.Navigation.Navigate (class Navigate)
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
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Page.Utils.Environment (getCurrentGenre)
import TuneBank.Api.Codec.TunesPage (TunesPage, decodeTunesPage)
import TuneBank.Api.Codec.Pagination (Pagination, defaultPagination, decodePagination)



currentUser = Nothing

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , searchParams :: SearchParams 
  , searchResult :: Either String (Tuple TunesPage Pagination)
  }

type Input =
  { searchParams :: SearchParams }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Initialize
component
   :: ∀ o m r
    . MonadAff m 
   => MonadAsk { session :: Session | r } m 
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
  initialState { searchParams } =
     { genre : Scandi
     , searchParams
     , searchResult : Left ""
     }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ header Nothing Home
      , HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Tune List" ]
      , footer
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      -- state <- get
      genre <- getCurrentGenre
      -- searchResults <- requestTuneSearch baseURL (show genre) state.searchResults
      H.modify_ (\state -> state { genre = genre } )
