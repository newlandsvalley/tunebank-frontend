module TuneBank.Page.TuneList where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Array (length, range)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import TuneBank.Api.Codec.Pagination (Pagination)
import TuneBank.Api.Codec.TunesPage (TunesPage, TuneRefArray)
import TuneBank.Api.Request (requestTuneSearch)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.TuneId (TuneId(..), decodeTuneIdURIComponent)
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Page.Utils.Environment (getBaseURL, getCorsBaseURL, getCurrentGenre, getUser)

type Slot = H.Slot Query Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , searchParams :: SearchParams
  , searchResult :: Either String (Tuple TunesPage Pagination)
  }

type Input =
  { searchParams :: SearchParams }

-- type Query = (Const Void)
data Query a =
  FetchResults a

type ChildSlots = ()

data Action
  = Initialize
  | GoToPage Int

maxPageLinks :: Int
maxPageLinks = 10

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
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , finalize = Nothing
        }
    }
  where

  initialState :: Input -> State
  initialState { searchParams } =
     { genre : Scandi
     , currentUser : Nothing
     , searchParams
     , searchResult : Left ""
     }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ header state.currentUser state.genre Home
      , HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text ("Tune List page " <> show state.searchParams.page) ]
      , renderSearchResult state
      , renderPagination state
      , footer
      ]

  renderSearchResult :: State -> H.ComponentHTML Action ChildSlots m
  renderSearchResult state =
    case state.searchResult of
      Left err ->
        HH.text err
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
          -- route = Tune tuneRef.uri tuneId
          route = Tune tuneId
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

  renderPagination :: State -> H.ComponentHTML Action ChildSlots m
  renderPagination state =
    case state.searchResult of
      Left err ->
        HH.text ""
      Right (Tuple tunesPage pagination) ->
        HH.ul
          []
          ( [ renderFirstPage pagination ] <>
              renderNumberedPageLinks pagination <>
            [ renderLastPage pagination ]
          )
    where

      renderFirstPage pagination =
        if (pagination.maxPages > maxPageLinks  && pagination.page > 1) then
          paginationItem 1 pagination.page
            [ HH.text "first" ]
        else
          HH.text "no first page needed"

      renderLastPage pagination =
        if (pagination.maxPages > maxPageLinks  && pagination.page < pagination.maxPages) then
          paginationItem pagination.maxPages pagination.page
            [ HH.text "last" ]
          else
            HH.text "no last page needed"

      renderNumberedPageLinks pagination =
        let
          pageLink n =
            paginationItem n pagination.page
              [ HH.text (show n) ]
          first =
            max 1 (pagination.page - (maxPageLinks / 2))
          last =
            min pagination.maxPages (first + maxPageLinks)
        in
          map pageLink (range first last)


  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      currentUser <- getUser
      H.modify_ (\state -> state { genre = genre
                                 , currentUser = currentUser } )
      _ <- handleQuery (FetchResults unit)
      pure unit
    GoToPage page -> do
      -- | This is a little awkward.  I had expected that all I needed to do in
      -- | order to have pagination was to navigate to the same page but with
      -- | the new page parameter.  Unfortunately, this does not work - the
      -- | route changes OK (as indicated by the URL displayed by the browser)
      -- | but the page does not refresh.  This is presumably a Halogen slot
      -- | issue that I don't yet fully understand.
      -- | so, as well as navigate, we refresh the page - presumably we just
      -- | stay in the same slot.
      state <- H.get
      let
        newSearchParams = state.searchParams { page = page}
      _ <- H.modify (\st -> st { searchParams = newSearchParams } )
      _ <- navigate $ TuneList newSearchParams
      _ <- handleQuery (FetchResults unit)
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    FetchResults next -> do
      state <- H.get
      -- live server testing only baseURL <- getCorsBaseURL
      baseURL <- getBaseURL
      searchResult <- requestTuneSearch baseURL (asUriComponent state.genre) state.searchParams
      H.modify_ (\st -> st { searchResult = searchResult } )
      pure (Just next)

paginationItem
  :: ∀ m.
  Int ->
  Int ->
  Array (H.ComponentHTML Action ChildSlots m) ->
  H.ComponentHTML Action ChildSlots m
paginationItem thisPage currentPage html =
  HH.div
    [ css "pagination-item" ]
    [ HH.button
      [ css $ guard (thisPage == currentPage) "current"
      , HE.onClick \_ -> Just (GoToPage thisPage)
      ]
      html
    ]
