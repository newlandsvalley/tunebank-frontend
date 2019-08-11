module TuneBank.Page.TuneList where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Abc.Metadata (thumbnail)
import Data.Abc.Parser (parse)
import Data.Array (index, length, mapWithIndex, range, unsafeIndex)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..), fromRight)
import Data.Formatter.DateTime (Formatter, parseFormatString, format)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Global (readFloat)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import TuneBank.Api.Codec.Pagination (Pagination)
import TuneBank.Api.Codec.TunesPage (TunesPage, TuneRefArray)
import TuneBank.Api.Request (requestTuneSearch)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..), decodeTuneIdURIComponent)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.Utils (css, safeHref, truncateTo)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre)
import VexFlow.Abc.Alignment (justifiedScoreConfig, rightJustify)
import VexFlow.Score (Renderer, clearCanvas, createScore, renderScore, initialiseCanvas, resizeCanvas)
import VexFlow.Types (Config)

type Slot = H.Slot Query Void

type State =
  { genre :: Genre
  , searchParams :: SearchParams
  , searchResult :: Either String (Tuple TunesPage Pagination)
  , vexRenderers :: Array Renderer
  }

type Input =
  { searchParams :: SearchParams }

-- type Query = (Const Void)
data Query a =
  FetchResults a        -- Fetch a page of results
  | InitializeVex a     -- Initialise Vex renderers on first reference
  | Thumbnail Int a     -- Add thumbnail for row 0 (and chain through the rest)
  | ClearThumbnails a   -- clear the thumbnails of the current page

type ChildSlots = ()

data Action
  = Initialize          -- initialise the TuneList Page with default values
  | GoToPage Int        -- go to results page n
  | AddThumbnails       -- add all thumbnails to this page

maxPageLinks :: Int
maxPageLinks = 10

maxRowsPerPage :: Int
maxRowsPerPage = 15

scale :: Number
scale = 0.6

canvasWidth :: Int
canvasWidth =
  500

-- The default config for each thumbnail image via Vexflow
-- This is an initial config with a small height which is
-- overridden when the image is justified to its actual dimensions
defaultThumbnailConfig :: Int -> Config
defaultThumbnailConfig index =
  { parentElementId : ("canvas" <> show index)
  , width : canvasWidth
  , height : 10       -- set to a small value so we can reduce to this between pages
  , scale : scale
  , isSVG : true      -- only use Canvas backends for debug
  }

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
     , searchParams
     , searchResult : Left ""
     , vexRenderers : []
     }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ renderSearchResult state
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
            HH.div_
              [
              HH.h4
                 [ css "center" ]
                 [HH.text ("page "
                           <> show state.searchParams.page
                           <> " of "
                           <> show pagination.maxPages
                           )
                 ]
              , renderTuneList state tunesPage.tunes
              , renderPagination pagination
              , renderAddThumbnailsButton state
              ]

  renderTuneList :: State -> TuneRefArray -> H.ComponentHTML Action ChildSlots m
  renderTuneList state tunes =
    let
      -- f :: forall w i. TuneRef -> HH.HTML w i
      f i tuneRef =
        let
          tuneId = decodeTuneIdURIComponent tuneRef.uri
          dateString = tsToDateString tuneRef.ts
        in
          tableRow tuneId dateString i
    in
      HH.div
        [css "tunelist"]
        [ HH.table_ $
          (mapWithIndex f tunes) <>
          (renderPhantomRows $ length tunes)
        ]
    where

      -- render the rows where we have results from the search
      tableRow :: TuneId -> String -> Int -> H.ComponentHTML Action ChildSlots m
      tableRow tuneId dateString index =
        let
          (TuneId {title,  tuneType}) = tuneId
          route :: Route
          route = Tune state.genre tuneId
          -- route = Tune state.genre tuneId
        in
          HH.tr
            [  ]
            [ HH.td
              []
              [ HH.a
                [ safeHref route ]
                [ HH.text $ truncateTo 36 title]
              ]
            , HH.td
              []
              [ HH.text tuneType]
            , HH.td
              []
              [ HH.text dateString]
            , HH.td
              []
              [ HH.div
                [ HP.id_ ("canvas" <> show index)
                , css "thumbnail"
                ]
                []
              ]
              {-}
              [ HH.canvas
                [ HP.id_ ("canvas" <> show index)
                , css "thumbnail"
                , HP.height 10
                , HP.width 500
                ]
              ]
              -}
            {-}
            , HH.td
              []
              [ HH.text $ debugHref route ]
            -}
            ]

      -- render further 'phantom' rows up to the max rows per page
      -- in order to give a regular number of rows for each page and thus
      -- preserve canvas slots for the thumbnails
      renderPhantomRows :: Int -> Array (H.ComponentHTML Action ChildSlots m)
      renderPhantomRows start =
        if
          start >= (maxRowsPerPage -1 ) then
            []
        else
          let
            rows = range start (maxRowsPerPage -1)
          in
            map renderPhantomRow rows


  renderPagination :: Pagination -> H.ComponentHTML Action ChildSlots m
  renderPagination pagination =
    HH.div
      [ css "pagination-div" ]
      [ HH.ul
          [ css "pagination"]
          ( [ renderFirstPage  ] <>
            [ renderPrevPage  ] <>
              renderNumberedPageLinks  <>
            [ renderNextPage  ] <>
            [ renderLastPage  ]
          )
      ]
    where

      renderFirstPage  =
        if (pagination.maxPages > maxPageLinks  && pagination.page > 1) then
          paginationItem 1 pagination.page
            [ HH.text "first" ]
        else
          HH.text ""

      renderLastPage  =
        if (pagination.maxPages > maxPageLinks  && pagination.page < pagination.maxPages) then
          paginationItem pagination.maxPages pagination.page
            [ HH.text "last" ]
          else
            HH.text ""

      renderPrevPage  =
        if (pagination.page > 1) then
          paginationItem (pagination.page -1) pagination.page
            [ HH.text "prev" ]
        else
          HH.text ""

      renderNextPage  =
        if (pagination.page < pagination.maxPages) then
          paginationItem (pagination.page + 1) pagination.page
            [ HH.text "next" ]
        else
          HH.text ""

      renderNumberedPageLinks  =
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

  renderAddThumbnailsButton :: ∀ m. State -> H.ComponentHTML Action ChildSlots m
  renderAddThumbnailsButton state =
      HH.button
        [ HE.onClick \_ -> Just AddThumbnails
        , css "hoverable"
        , HP.enabled true
        ]
        [ HH.text "add thumbnails" ]


  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      H.modify_ (\state -> state { genre = genre } )
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
      _ <- handleQuery (ClearThumbnails unit)
      _ <- handleQuery (FetchResults unit)
      pure unit
    AddThumbnails -> do
      _ <- handleQuery (InitializeVex unit)
      _ <- handleQuery (Thumbnail 0 unit)
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    -- Fetch the results of the search for the page in question
    FetchResults next -> do
      state <- H.get
      -- live server testing only baseURL <- getCorsBaseURL
      baseURL <- getBaseURL
      searchResult <- requestTuneSearch baseURL (asUriComponent state.genre) state.searchParams
      H.modify_ (\st -> st { searchResult = searchResult } )
      -- handleQuery (InitializeVex next)
      pure (Just next)

    -- Initialization of the Vex rendere which is done on first reference.
    -- Note, we can obly initialising after rendering the page for the first time
    -- because only then are the canvas Div elements established
    InitializeVex next -> do
      state <- H.get
      let
        foo = spy "INITIALIZEVEX" (length state.vexRenderers)
      if (length state.vexRenderers > 0)
        then do
          -- already initialized
          pure (Just next)
        else do
          let
            rows :: Array Int
            rows = range 0 (maxRowsPerPage - 1)
          renderers <- H.liftEffect $ traverse (\r -> initialiseCanvas $ defaultThumbnailConfig r) rows
          H.modify_ (\st -> st { vexRenderers = renderers } )
          -- _ <- handleQuery (Thumbnail 0 unit)
          pure (Just next)

    -- render the thumbnail at index idx
    Thumbnail idx next -> do
      let
        bazz =
          spy "Thumbnail Query for index: " idx
      state <- H.get
      case state.searchResult of
        Left err ->
          pure (Just next)
        Right (Tuple tunesPage pagination) ->
          if (idx >= (length $ tunesPage.tunes) )
            then do
              let
                bar =
                  spy "thumbnail index out of range: " idx
              pure (Just next)
            else do
              let
                tuneRef = unsafePartial $ unsafeIndex tunesPage.tunes idx
              case (Tuple (parse tuneRef.abc) (index state.vexRenderers idx)) of
                (Tuple (Right abcTune) (Just renderer)) -> do
                  _ <- H.liftEffect $ clearCanvas renderer
                  let
                    foo =
                      spy "rendering thumbnail for" idx
                    unjustifiedScore = createScore (defaultThumbnailConfig idx) (thumbnail abcTune)
                    score = rightJustify canvasWidth scale unjustifiedScore
                    config = justifiedScoreConfig score (defaultThumbnailConfig idx)
                  _ <- H.liftEffect $ resizeCanvas renderer config
                  _ <- H.liftEffect $ renderScore config renderer score
                  -- try to force a re-render after each row
                  H.modify_ (\st -> st { genre = state.genre } )
                  handleQuery (Thumbnail (idx + 1) next)
                (Tuple (Right _) Nothing)  -> do
                  let
                    baz =
                      spy "no renderer for index: " idx
                  handleQuery (Thumbnail (idx + 1) next)
                (Tuple (Left _) _)  -> do
                  let
                    baz =
                      spy "tuneRef not parsed for index: " idx
                    baz1 =
                        spy "abc: " tuneRef.abc
                  handleQuery (Thumbnail (idx + 1) next)

    ClearThumbnails next -> do
      state <- H.get
      let
        f :: Int -> Renderer -> Effect Unit
        f i renderer = resizeCanvas renderer (defaultThumbnailConfig i)
      _ <- H.liftEffect $ traverseWithIndex f state.vexRenderers
      _ <- H.liftEffect $ traverse (clearCanvas) state.vexRenderers
      pure (Just next)

paginationItem
  :: ∀ m.
  Int ->
  Int ->
  Array (H.ComponentHTML Action ChildSlots m) ->
  H.ComponentHTML Action ChildSlots m
paginationItem thisPage currentPage html =
  HH.li
    [ css "pagination-item" ]
    [ HH.a
      [ css $ guard (thisPage == currentPage) "current"
      , HE.onClick \_ -> Just (GoToPage thisPage)
      ]
      html
    ]

tsToDateString :: String-> String
tsToDateString tsString =
  let
     mInstant = instant $ Milliseconds $ readFloat tsString
     dateTime = maybe (bottom) (toDateTime) mInstant
     displayFormatter :: Formatter
     displayFormatter =  unsafePartial fromRight $ parseFormatString "DD MMM YYYY"
  in
    format displayFormatter dateTime

resultRows :: Either String (Tuple TunesPage Pagination) -> Int
resultRows = case _ of
  Right (Tuple tunePage pagination) ->
    length tunePage.tunes
  _ ->
    0

-- | if we have less than a full page of 15 rows, generate phantoms
-- | for the rest with workable canvas references
renderPhantomRow :: ∀ i p. Int -> HH.HTML i p
renderPhantomRow index =
  HH.tr
    [ ]
    [ HH.td
      []
      [ HH.text ""]
    , HH.td
      []
      [ HH.text ""]
    , HH.td
      []
      [ HH.text ""]
    , HH.td
      []
      [ HH.div
        [ HP.id_ ("canvas" <> show index)
        , css "thumbnail"
        ]
        []
      ]
      {-
      [ HH.canvas
        [ HP.id_ ("canvas" <> show index)
        , css "thumbnail"
        , HP.height 10
        , HP.width 500
        ]
      ]
      -}
    ]
