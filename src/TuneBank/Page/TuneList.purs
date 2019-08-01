module TuneBank.Page.TuneList where

import Prelude
import Global (readFloat)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (index, length, mapWithIndex, range, unsafeIndex)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..))
import Data.Formatter.DateTime (Formatter, parseFormatString, format)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Abc.Parser (parse)
import Data.Abc.Metadata (thumbnail)
import VexFlow.Types (Config)
import VexFlow.Score (Renderer, clearCanvas, createScore, renderScore, initialiseCanvas, resizeCanvas)
import VexFlow.Abc.Alignment (justifiedScoreConfig, rightJustify)
import TuneBank.Api.Codec.Pagination (Pagination)
import TuneBank.Api.Codec.TunesPage (TunesPage, TuneRefArray)
import TuneBank.Api.Request (requestTuneSearch)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.TuneId (TuneId(..), decodeTuneIdURIComponent)
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams)
import TuneBank.Page.Utils.Environment (getBaseURL, getCorsBaseURL, getCurrentGenre)


import Debug.Trace (spy, trace)

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
  FetchResults a
  | InitializeVex a
  | Thumbnail Int a

type ChildSlots = ()

data Action
  = Initialize
  | GoToPage Int

maxPageLinks :: Int
maxPageLinks = 10

scale :: Number
scale = 0.6

canvasWidth :: Int
canvasWidth =
  900

defaultVexConfig :: Int -> Config
defaultVexConfig index =
  { canvasDivId : ("canvas" <> show index)
  , canvasWidth : canvasWidth
  , canvasHeight : 70
  , scale : scale
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
              , renderPagination pagination
              , HH.h5_ [ HH.text "..."]
              , renderTuneList state tunesPage.tunes
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
          mapWithIndex f tunes
        ]
    where

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
                [ HH.text title]
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
            , HH.td
              []
              [ HH.text $ debugHref route ]
            -}
            ]



  renderPagination :: Pagination -> H.ComponentHTML Action ChildSlots m
  renderPagination pagination =
    HH.div_
      [ HH.ul
          [ css "pagination"]
          ( [ renderFirstPage  ] <>
              renderNumberedPageLinks  <>
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
      _ <- handleQuery (InitializeVex unit)
      _ <- handleQuery (Thumbnail 0 unit)
      pure (Just next)

    InitializeVex next -> do
      -- Initialization of the Vex rendere which is done on first reference.
      -- Note, we can obly initialising after rendering the page for the first time
      -- because only then are the canvas Div elements established
      state <- H.get
      if (length state.vexRenderers > 0)
        then do
          -- already initialized
          _ <- handleQuery (Thumbnail 0 unit)
          pure (Just next)
        else do
          case (resultRows state.searchResult) of
            0 ->
              -- we need some arbitrary statement here
              H.modify_ (\st -> st { vexRenderers = state.vexRenderers } )
            n -> do
              -- initialise as many renderers as we have result rows
              let
                rows :: Array Int
                rows = range 0 (n - 1)
              renderers <- H.liftEffect $ traverse (\r -> initialiseCanvas $ defaultVexConfig r) rows
              H.modify_ (\st -> st { vexRenderers = renderers } )
          -- _ <- handleQuery (Thumbnail 0 unit)
          pure (Just next)

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
                    unjustifiedScore = createScore (defaultVexConfig idx) (thumbnail abcTune)
                    score = rightJustify canvasWidth scale unjustifiedScore
                    config = justifiedScoreConfig score (defaultVexConfig idx)
                  _ <- H.liftEffect $ resizeCanvas renderer config
                  _ <- H.liftEffect $ renderScore config renderer score
                  _ <- handleQuery (Thumbnail (idx + 1) unit)
                  -- try to force a re-render after each row
                  H.modify_ (\st -> st { genre = state.genre } )
                  pure (Just next)
                  -- Thumbnail (index + 1) next
                _ -> do
                  let
                    baz =
                      spy "no renderer or tuneRef not parsed for index: " idx
                    baz1 =
                        spy "abc: " tuneRef.abc
                  _ <- handleQuery (Thumbnail (idx + 1) unit)
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
