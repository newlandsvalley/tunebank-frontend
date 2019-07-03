module TuneBank.Page.TuneList where

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array (length)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<>), bind, map, pure, show, unit)
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Api.Codec.Pagination (Pagination, defaultPagination, decodePagination)
import TuneBank.Api.Codec.TunesPage (TunesPage, TuneRef, TuneRefArray)
import TuneBank.Api.Request (requestTuneSearch)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL, TuneId(..), decodeTuneIdURIComponent)
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre)

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
      Right (Tuple tunesPage pagination) ->
        case (length tunesPage.tunes) of
          0 ->
             HH.text "no matching tunes found"
          _ ->
             renderTuneList tunesPage.tunes


  renderTuneList :: TuneRefArray -> H.ComponentHTML Action ChildSlots m
  renderTuneList tunes =
    let
      -- f :: forall w i. TuneRef -> HH.HTML w i
      f tuneRef =
        let
          tuneId = decodeTuneIdURIComponent tuneRef.uri
          (TuneId {title,  tuneType}) = tuneId
          route :: Route
          route = Tune "Scandi" tuneId
        in
          linkItem route
            [ HH.text $ show title ]
    in
      HH.ul_ $
        map f tunes
    where

      linkItem r html =
        HH.li
          [ css "link-item" ]
          [ HH.a
            [ -- css $ guard (route == r) "current"
              safeHref r
            ]
            html
          ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      genre <- getCurrentGenre
      baseURL <- getBaseURL
      searchResult <- requestTuneSearch baseURL (asUriComponent genre) state.searchParams
      H.modify_ (\state -> state { genre = genre, searchResult = searchResult } )
