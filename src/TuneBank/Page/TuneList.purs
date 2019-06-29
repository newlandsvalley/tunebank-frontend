module TuneBank.Page.TuneList where

import Prelude (Unit, Void, ($), (<>), bind, pure, show, unit)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk, asks)
import TuneBank.Navigation.Navigate (class Navigate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TuneBank.HTML.Header (header)
import TuneBank.HTML.Footer (footer)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre)
import TuneBank.Api.Codec.TunesPage (TunesPage, decodeTunesPage)
import TuneBank.Api.Codec.Pagination (Pagination, defaultPagination, decodePagination)
import TuneBank.Api.Request (requestTuneSearch)

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
      , renderSearchResult state
      , footer
      ]

  renderSearchResult :: State -> H.ComponentHTML Action ChildSlots m
  renderSearchResult state =
    case state.searchResult of
      Left err ->
        HH.text ("tune search error: " <> err )
      Right tuple ->
        HH.text "tune search success"

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      genre <- getCurrentGenre
      baseURL <- getBaseURL
      searchResult <- requestTuneSearch baseURL (show genre) state.searchParams
      H.modify_ (\state -> state { genre = genre, searchResult = searchResult } )
