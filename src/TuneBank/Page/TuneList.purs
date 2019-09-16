module TuneBank.Page.TuneList where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Abc.Metadata (thumbnail, removeRepeatMarkers)
import Data.Abc.Parser (parse)
import Data.Abc.Midi (toMidi)
import Data.Array (index, length, mapWithIndex, range, unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (delay)
import Data.Time.Duration (Milliseconds(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.ThumbnailPlayerComponent (Query(..), Slot, component) as TNP
import Partial.Unsafe (unsafePartial)
import TuneBank.Api.Codec.Pagination (Pagination)
import TuneBank.Api.Codec.TunesPage (TunesPage, TuneRefArray)
import TuneBank.Api.Request (requestTuneSearch)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..), decodeTuneIdURIComponent)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.Utils (css, safeHref, truncateTo, tsToDateString)
import TuneBank.HTML.PaginationRendering  (renderPagination)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams, paramsSummary)
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre)
import VexFlow.Abc.Alignment (justifiedScoreConfig, rightJustify)
import VexFlow.Score (Renderer, clearCanvas, createScore, renderScore, initialiseCanvas, resizeCanvas)
import VexFlow.Types (Config)
import Audio.SoundFont (Instrument)
import Audio.SoundFont.Melody (Melody)
import Audio.SoundFont.Melody.Maker (toMelody_)

type Slot = H.Slot Query Void

type State =
  { genre :: Genre
  , searchParams :: SearchParams
  , searchResult :: Either String (Tuple TunesPage Pagination)
  , instruments :: Array Instrument
  , vexRenderers :: Array Renderer
  , hasThumbnails :: Boolean
  , selectedThumbnail :: Maybe Int
  }

type Input =
  { searchParams :: SearchParams
  , instruments :: Array Instrument
  }

-- type Query = (Const Void)
data Query a =
  FetchResults a        -- Fetch a page of results
  | InitializeVex a     -- Initialise Vex renderers on first reference
  | Thumbnail Int a     -- Add thumbnail for row 0 (and chain through the rest)
  | ClearThumbnails a   -- clear the thumbnails of the current page

type ChildSlots =
  ( thumbnailPlayer :: TNP.Slot Unit )

_thumbnailPlayer = SProxy :: SProxy "thumbnailPlayer"

data Action
  = Initialize               -- initialise the TuneList Page with default values
  | HandleInput Input
  | AddThumbnails            -- add all thumbnails to this page
  | HighlightThumbnail Int   -- when the thumbnail has mouse enter
  | PlayThumbnail Int        -- when the thumbnail is cliecked
  | StopThumbnail            -- when the thumbnail has mouse leave

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
        , receive = Just <<< HandleInput
        , initialize = Just Initialize
        , finalize = Nothing
        }
    }
  where

  initialState :: Input -> State
  initialState { searchParams, instruments } =
     { genre : Scandi
     , searchParams
     , searchResult : Left ""
     , instruments
     , vexRenderers : []
     , hasThumbnails : false
     , selectedThumbnail : Nothing
     }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.slot _thumbnailPlayer unit TNP.component { instruments : state.instruments } absurd
      , renderSearchResult state
      ]

  renderSearchResult :: State -> H.ComponentHTML Action ChildSlots m
  renderSearchResult state =
    case state.searchResult of
      Left err ->
        HH.text err
      Right (Tuple tunesPage pagination) ->
        case (length tunesPage.tunes) of
          0 ->
             HH.div
               [ css "center" ]
               [HH.h4_
                  [ HH.text ( "search results for: "
                           <> paramsSummary state.searchParams
                          )
                  ]
                , HH.p_
                    [HH.text "no matching tunes found"]
               ]
          _ ->
            HH.div_
              [
              HH.h4
                 [ css "center" ]
                 [HH.text ( "search results for: "
                            <> paramsSummary state.searchParams
                            <> " page "
                            <> show state.searchParams.page
                            <> " of "
                            <> show pagination.maxPages
                           )
                 ]
              , renderTuneList state tunesPage.tunes
              -- , renderPagination pagination
              , renderPagination (TuneList state.searchParams) pagination
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
              [ renderThumbnailCanvas index ]
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

      -- render the thumbnail canvas.  This canvas is empty, but populated by
      -- side-effect if AddThumbnails is pressed.  We don't want to have
      -- active onMouse functions if the thumbnail is inactive and we highlight
      -- the thumbnail if the mpuse passes over it
      renderThumbnailCanvas :: Int -> H.ComponentHTML Action ChildSlots m
      renderThumbnailCanvas index =
        let
          -- highlight the currently hovered over thumbnail image
          f target =
            if (index == target) then
              "thumbnail-highlighted"
            else
              "thumbnail"
          cssSelector = maybe "thumbnail" f state.selectedThumbnail
        in
          if (state.hasThumbnails) then
            HH.div
              [ HP.id_ ("canvas" <> show index)
              , css cssSelector
              , HE.onMouseOver \_ -> Just (HighlightThumbnail index)
              , HE.onMouseLeave \_ -> Just StopThumbnail
              , HE.onMouseDown \_ -> Just (PlayThumbnail index)
              ]
              []
          else
            HH.div
              [ HP.id_ ("canvas" <> show index)
              , css "thumbnail"
              ]
              []

  renderAddThumbnailsButton :: State -> H.ComponentHTML Action ChildSlots m
  renderAddThumbnailsButton state =
    if (state.hasThumbnails) then
      HH.text ""
    else
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

    HandleInput input -> do
      H.modify_ (\st -> st { searchParams = input.searchParams } )
      _ <- handleQuery (ClearThumbnails unit)
      _ <- handleQuery (FetchResults unit)
      pure unit

    AddThumbnails -> do
      _ <- handleQuery (InitializeVex unit)
      _ <- handleQuery (Thumbnail 0 unit)
      _ <- H.modify_ (\state -> state { hasThumbnails = true } )
      pure unit

    HighlightThumbnail idx -> do
      _ <- H.modify_ (\state -> state { selectedThumbnail = Just idx } )
      pure unit

    PlayThumbnail idx -> do
      _ <- H.query _thumbnailPlayer unit $ H.tell TNP.StopMelody
      state <- H.get
      case state.searchResult of
        Left err ->
          pure unit
        Right (Tuple tunesPage pagination) ->
          if (idx >= (length $ tunesPage.tunes) )
            then do
              pure unit
            else do
              let
                tuneRef = unsafePartial $ unsafeIndex tunesPage.tunes idx
                melody = getThumbnailMelody tuneRef.abc
              _ <- H.query _thumbnailPlayer unit $ H.tell (TNP.PlayMelody melody)
              pure unit

    StopThumbnail -> do
      _ <- H.modify_ (\state -> state { selectedThumbnail = Nothing } )
      _ <- H.query _thumbnailPlayer unit $ H.tell TNP.StopMelody
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
      -- we delay here to give the UI chance to re-render between each thumbnail
      _ <- H.liftAff $ delay (Milliseconds 25.0 )
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
                  handleQuery (Thumbnail (idx + 1) next)

    ClearThumbnails next -> do
      state <- H.get
      _ <- H.modify_ (\st -> st { hasThumbnails = false } )
      let
        f :: Int -> Renderer -> Effect Unit
        f i renderer = resizeCanvas renderer (defaultThumbnailConfig i)
      _ <- H.liftEffect $ traverseWithIndex f state.vexRenderers
      _ <- H.liftEffect $ traverse (clearCanvas) state.vexRenderers
      pure (Just next)

resultRows :: Either String (Tuple TunesPage Pagination) -> Int
resultRows = case _ of
  Right (Tuple tunePage pagination) ->
    length tunePage.tunes
  _ ->
    0

-- | if we have less than a full page of 15 rows, generate phantoms
-- | for the rest with workable but empty canvas references
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
    ]

getThumbnailMelody :: String -> Melody
getThumbnailMelody abc =
  case (parse abc) of
    Right abcTune ->
      (toMelody_ 0.50 <<< toMidi <<< removeRepeatMarkers <<< thumbnail) abcTune
    _ ->
      []
