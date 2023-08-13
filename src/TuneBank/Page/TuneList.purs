module TuneBank.Page.TuneList where

import Prelude

import Audio.SoundFont (Instrument)
import Audio.SoundFont.Melody (Melody)
import Control.Monad.Reader (class MonadAsk)
import Data.Abc.Melody (PlayableAbc(..), defaultPlayableAbcProperties, toPlayableMelody)
import Data.Abc.Parser (parse)
import Data.Abc.Utils (thumbnail, removeRepeatMarkers)
import Data.Array (index, length, mapWithIndex, range, unsafeIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.ThumbnailPlayerComponent (Query(..), Slot, component) as TNP
import Partial.Unsafe (unsafePartial)
import TuneBank.Api.Codec.TunesPage (TunesPage, TuneRef, TuneRefArray)
import TuneBank.Api.Request (requestTuneSearch)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..), decodeTuneIdURIComponent)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.PaginationRendering (renderPagination)
import TuneBank.HTML.Utils (css, safeHref, truncateTo, tsToDateString)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams, paramsSummary)
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre)
import Type.Proxy (Proxy(..))
import VexFlow.Score (Renderer, clearCanvas, initialiseCanvas, renderThumbnail, resizeCanvas)
import VexFlow.Types (Config, Titling(..), defaultConfig)
import Web.HTML (window) as HTML
import Web.HTML.Window (innerWidth) as Window

type Slot = H.Slot Query Void

type State =
  { genre :: Genre
  , searchParams :: SearchParams
  , searchResult :: Either String TunesPage
  , instruments :: Array Instrument
  , vexRenderers :: Array Renderer
  , hasThumbnails :: Boolean
  , selectedThumbnail :: Maybe Int
  , windowWidth :: Int
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

_thumbnailPlayer = Proxy :: Proxy "thumbnailPlayer"

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
  defaultConfig 
    { parentElementId = ("canvas" <> show index)
    , width = canvasWidth
    , height = 10
    , isSVG = true 
    , scale = scale
    , titling = NoTitle
    }

component
   :: ∀ o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
   => Navigate m
   => H.Component Query Input o m
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
     , windowWidth : 0
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
      Right tunesPage ->
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
                            <> show tunesPage.pagination.page
                            <> " of "
                            <> show tunesPage.pagination.maxPages
                           )
                 ]
              , renderTuneList state tunesPage.tunes
              , renderPagination (TuneList state.searchParams) tunesPage.pagination
              , renderAddThumbnailsButton state
              ]

  renderTuneList :: State -> TuneRefArray -> H.ComponentHTML Action ChildSlots m
  renderTuneList state tunes =
    HH.div
      [css "tunelist"]
      [ HH.table_ $
        (mapWithIndex buildRow tunes) <>
        (renderPhantomRows $ length tunes)
      ]

    where

      isLargeScreen = state.windowWidth > 600        
      
      -- build one row of the tune list table
      buildRow :: Int -> TuneRef -> H.ComponentHTML Action ChildSlots m
      buildRow i tuneRef =
        let
          tuneId = decodeTuneIdURIComponent tuneRef.uri
          dateString = tsToDateString tuneRef.ts
        in
          tableRow tuneId dateString i

      -- render a row where we have results from the search
      -- on large screens we display the tune type and date columns, on mobiles, we don't
      tableRow :: TuneId -> String -> Int -> H.ComponentHTML Action ChildSlots m
      tableRow tuneId dateString index =
        HH.tr
          [  ]
          ( [ HH.td
              []
              [ HH.a
                [ safeHref route ]
                [ HH.text $ truncateTo 36 title]
              ]
            ]

            <>
            -- maybe include tune type and date
            tuneTypeAndDateCells isLargeScreen tuneType dateString

            <>

            [ HH.td
              []
              [ renderThumbnailCanvas index ]
            ]
          )
        where
          (TuneId {title,  tuneType}) = tuneId
          -- route = Tune state.genre tuneId
          route :: Route
          route = Tune state.genre tuneId

      -- render further 'phantom' rows up to the max rows per page
      -- in order to give a regular number of rows for each page and thus
      -- preserve canvas slots for the thumbnails
      renderPhantomRows :: Int -> Array (H.ComponentHTML Action ChildSlots m)
      renderPhantomRows start  =
        if
          start >= (maxRowsPerPage -1 ) then
            []
        else
          let
            rows = range start (maxRowsPerPage -1)
          in
            map renderPhantomRow rows

      -- | if we have less than a full page of 15 rows, generate phantoms
      -- | for the rest with workable but empty canvas references
      renderPhantomRow :: ∀ i p. Int -> HH.HTML i p
      renderPhantomRow index =
        HH.tr
          [ ]
          ( [ HH.td
              []
              [ HH.text ""]
            ]

            <> 

            tuneTypeAndDateCells isLargeScreen "" ""

            <>

            [ HH.td
              []
              [ HH.div
                [ HP.id ("canvas" <> show index)
                , css "thumbnail"
                ]
                []
              ]
            ] 
          )


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
              [ HP.id ("canvas" <> show index)
              , css cssSelector
              , HE.onMouseOver \_ -> HighlightThumbnail index
              , HE.onMouseLeave \_ -> StopThumbnail
              , HE.onMouseDown \_ -> PlayThumbnail index
              ]
              []
          else
            HH.div
              [ HP.id ("canvas" <> show index)
              , css "thumbnail"
              ]
              []

  renderAddThumbnailsButton :: State -> H.ComponentHTML Action ChildSlots m
  renderAddThumbnailsButton state =
    if (state.hasThumbnails) then
      HH.text ""
    else
      HH.div 
        [ HP.id "add-thumbnails" ]
        [ HH.button
          [ HE.onClick \_ -> AddThumbnails
          , css "hoverable"
          , HP.enabled true
          ]
          [ HH.text "add thumbnails" ]
        ]


  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      window <- H.liftEffect HTML.window
      windowWidth <- H.liftEffect $ Window.innerWidth window
      H.modify_ (\state -> state 
        { genre = genre
        , windowWidth = windowWidth 
        } )
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
      -- play the thumbnail unless the thumbnail player is still playing
      isPlaying <- H.request _thumbnailPlayer unit TNP.IsPlaying
      -- isPlaying <- H.query _thumbnailPlayer unit $ H.request TNP.IsPlaying
      state <- H.get
      case state.searchResult of
        Left _err ->
          pure unit
        Right tunesPage  ->
          if ((idx >= (length $ tunesPage.tunes) ) || ( isPlaying == Just true))
            then do
              pure unit
            else do
              let
                tuneRef = unsafePartial $ unsafeIndex tunesPage.tunes idx
                melody = getThumbnailMelody tuneRef.abc
              _ <- H.tell _thumbnailPlayer unit (TNP.PlayMelody melody)
              -- _ <- H.query _thumbnailPlayer unit $ H.tell (TNP.PlayMelody melody)
              pure unit

    StopThumbnail -> do
      _ <- H.modify_ (\state -> state { selectedThumbnail = Nothing } )
      _ <- H.tell _thumbnailPlayer unit TNP.StopMelody
      -- _ <- H.query _thumbnailPlayer unit $ H.tell TNP.StopMelody
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
      state <- H.get
      case state.searchResult of
        Left _err ->
          pure (Just next)
        Right tunesPage ->
          if (idx >= (length $ tunesPage.tunes) )
            then do
              pure (Just next)
            else do
              let
                tuneRef = unsafePartial $ unsafeIndex tunesPage.tunes idx
              case (Tuple (parse tuneRef.abc) (index state.vexRenderers idx)) of
                (Tuple (Right abcTune) (Just renderer)) -> do
                  let 
                    config = defaultThumbnailConfig idx
                  _ <- H.liftEffect $ renderThumbnail config renderer abcTune 
                  -- try to force a re-render after each row
                  H.modify_ (\st -> st { genre = state.genre } )
                  handleQuery (Thumbnail (idx + 1) next)
                (Tuple (Right _) Nothing)  -> do
                  handleQuery (Thumbnail (idx + 1) next)
                (Tuple (Left _) _)  -> do
                  handleQuery (Thumbnail (idx + 1) next)

    ClearThumbnails next -> do
      state <- H.get
      _ <- H.modify_ (\st -> st { hasThumbnails = false } )
      let
        f :: Int -> Renderer -> Effect Renderer
        f i renderer = resizeCanvas renderer (defaultThumbnailConfig i)
      _ <- H.liftEffect $ traverseWithIndex f state.vexRenderers
      _ <- H.liftEffect $ traverse (clearCanvas) state.vexRenderers
      pure (Just next)

resultRows :: Either String TunesPage -> Int
resultRows = case _ of
  Right tunePage  ->
    length tunePage.tunes
  _ ->
    0



getThumbnailMelody :: String -> Melody
getThumbnailMelody abc =
  case (parse abc) of
    Right abcTune ->
      let
        thumbnailTune = (removeRepeatMarkers <<< thumbnail) abcTune
        props = defaultPlayableAbcProperties { tune = thumbnailTune }
        playableAbc = PlayableAbc props
      in
        toPlayableMelody playableAbc
    _ ->
      []

-- include the tune type and date cells dependent on whether or not it's a large screen
tuneTypeAndDateCells :: forall i p. Boolean -> String -> String-> Array (HH.HTML i p)
tuneTypeAndDateCells isLargeScreen tuneTypeString tuneDateString =
  if isLargeScreen then
    [ HH.td
        []
        [ HH.text tuneTypeString]
    , HH.td
        []
        [ HH.text tuneDateString]
    ]
  else
    []
