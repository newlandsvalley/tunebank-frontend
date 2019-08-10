module TuneBank.Page.Tune where

import Prelude

import Audio.SoundFont (Instrument)
import Audio.SoundFont.Melody.Class (MidiRecording(..))
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Abc (AbcTune)
import Data.Abc.Midi (toMidi)
import Data.Abc.Parser (parse)
import Data.Array (filter, length)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Symbol (SProxy(..))
import Data.Link (expandLinks)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.PlayerComponent as PC
import Html.Renderer.Halogen as RH
import TuneBank.Api.Codec.Comments (Comments, Comment)
import TuneBank.Api.Codec.Tune (TuneMetadata, nullTuneMetadata)
import TuneBank.Api.Request (requestTune, requestComments, deleteComment)
import TuneBank.Data.CommentId (CommentId)
import TuneBank.Data.Credentials (Credentials, Role(..))
import TuneBank.Data.Genre (Genre, asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (TuneId(..), encodeTuneIdURIComponent)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getInstruments, getUser)

-- | there is no tune yet
nullParsedTune :: Either String AbcTune
nullParsedTune =
  Left "no tune yet"

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , tuneURI :: String
  , tuneId :: TuneId
  , baseURL :: BaseURL
  , tuneMetadata :: TuneMetadata
  , tuneResult :: Either String AbcTune
  , comments :: Comments
  , instruments :: Array Instrument
  }

type Input =
  { genre :: Genre
  , tuneId :: TuneId
  }

type Query = (Const Void)

type ChildSlots =
   (player :: (PC.Slot MidiRecording) Unit)

_player = SProxy :: SProxy "player"

data Action
  = Initialize
  | HandleTuneIsPlaying PC.Message
  | DeleteComment CommentId

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
  initialState input =
    { genre : input.genre
    , currentUser : Nothing
    , tuneURI : encodeTuneIdURIComponent input.tuneId
    , tuneId : input.tuneId
    , baseURL : BaseURL ""
    , tuneMetadata : nullTuneMetadata
    , tuneResult: nullParsedTune
    , comments : []
    , instruments : []
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    let
      (TuneId {title, tuneType}) = state.tuneId
    in
      HH.div_
        [ HH.h1
           [HP.class_ (H.ClassName "center") ]
           [HH.text title ]
        , renderTuneScore state title
        , renderTuneMetadata state
        , renderPlayer state
        , renderComments state
        , renderDebug state
        ]

  renderTuneScore :: State -> String -> H.ComponentHTML Action ChildSlots m
  renderTuneScore state title =
    let
      imageURI = urlPreface state <> "/png"
    in
      HH.div_
        [HH.img
          [ HP.src imageURI
          , HP.alt title
          ]
        ]

  renderTuneMetadata :: State -> H.ComponentHTML Action ChildSlots m
  renderTuneMetadata state  =
    HH.dl
      []
      [ renderTuneSubmitter state
      , HH.dt
         []
         [ HH.text "download"]
      , HH.dd
         []
         [ HH.a
            [ HP.href (urlPreface state <> "/abc")
            , HP.type_ (MediaType "text/vnd.abc")
            ]
            [ HH.text "abc"]

         , HH.a
            [ HP.href (urlPreface state <> "/pdf")
            , HP.type_ (MediaType "application/pdf")
            ]
            [ HH.text "pdf"]

         , HH.a
            [ HP.href (urlPreface state <> "/ps")
            , HP.type_ (MediaType "application/postscript")
            ]
            [ HH.text "postscript"]

          , HH.a
            [ HP.href (urlPreface state <> "/midi")
            , HP.type_ (MediaType "audio/midi")
            ]
            [ HH.text "midi"]
         ]
         , renderAddComment state
      ]

  renderTuneSubmitter :: State -> H.ComponentHTML Action ChildSlots m
  renderTuneSubmitter state =
    HH.div_
      [
      HH.dt
        []
        [ HH.text "submitter" ]
      , HH.dd
        []
        [ HH.text state.tuneMetadata.submitter ]
      ]

  renderAddComment :: State -> H.ComponentHTML Action ChildSlots m
  renderAddComment state =
    HH.div_
      [
      HH.dt
        []
        [ HH.text "tune" ]
      , HH.dd
        []
        [ HH.a
           [ safeHref $ Comments state.genre state.tuneId ]
           [ HH.text "add comment"]
        ]
      ]

  renderPlayer ::  State -> H.ComponentHTML Action ChildSlots m
  renderPlayer state =
    case state.tuneResult of
      Right abcTune ->
        HH.div
          [ HP.class_ (H.ClassName "leftPanelComponent")
          , HP.id_  "player-div"
          ]
          [ HH.slot _player unit (PC.component (toPlayable abcTune) state.instruments) unit (Just <<< HandleTuneIsPlaying) ]
      Left err ->
        HH.div_
          [  ]
      where
        -- | convert a tune to a format recognized by the player
        toPlayable :: AbcTune -> MidiRecording
        toPlayable abcTune =
          MidiRecording $ toMidi abcTune

  renderComments ::  State -> H.ComponentHTML Action ChildSlots m
  renderComments state =
    let
      commentCount = length state.comments
      header =
        case commentCount of
          0 ->
            "No comments"
          1 ->
            "1 comment"
          _ ->
            show commentCount <> " comments"
    in
    HH.div_
      [
        HH.text header
      , HH.div_ $ map (renderComment state) state.comments
      ]

  renderComment :: State -> Comment -> H.ComponentHTML Action ChildSlots m
  renderComment state comment =
    let
      editable =
        case state.currentUser of
          Just credentials ->
            (Administrator == credentials.role) || (comment.user == credentials.user)
          Nothing ->
            false
    in
      HH.div
        []
        [ HH.h2
          []
          [ HH.text comment.subject]
        , RH.render_ $ expandLinks comment.text
        --, HH.text comment.text
        , renderCommentControls state editable comment
        ]

  renderCommentControls :: State -> Boolean -> Comment -> H.ComponentHTML Action ChildSlots m
  renderCommentControls state isEditable comment =
    case isEditable of
      true ->
        HH.div_
          [ HH.a
            [ css "a-internal-link"
            , HE.onClick \_ -> Just $ DeleteComment comment.commentId
            ]
            [ HH.text "delete comment" ]
          , HH.a
            [ safeHref $ Comment state.genre state.tuneId comment.user comment.commentId ]
            [ HH.text "edit comment" ]
          ]
      false ->
        HH.text ""

  renderDebug ::  State -> H.ComponentHTML Action ChildSlots m
  renderDebug state =
    let
      instrumentCount = length state.instruments
      tuneResult = either identity (\_ -> "tune OK") state.tuneResult
    in
      HH.div_
        [
          HH.text (" tune result: " <> tuneResult)
        ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      -- but the full URL won't render!!!
      -- we save the genre to Ref state to cater for instances where we want
      -- to share tune URLS and ensure that both the user sees the right genre
      -- and can carry on browsing with that genre as the default
      session <- asks _.session
      _ <- H.liftEffect $ Ref.write state.genre session.genre
      currentUser <- getUser
      baseURL <- getBaseURL
      -- corsBaseURL <- getCorsBaseURL only temporary for live server
      instruments <- getInstruments
      -- tuneMetadataResult <- requestCleanTune baseURL (asUriComponent genre) state.tuneId
      tuneMetadataResult <- requestTune baseURL state.genre state.tuneId
      let
        tuneResult =
          tuneMetadataResult >>= (\x -> lmap show $ parse x.abc)
        tuneMetadata =
          either (const state.tuneMetadata) identity tuneMetadataResult
      comments <- requestComments baseURL state.genre state.tuneId
      H.modify_ (\st -> st
        { currentUser = currentUser
        , baseURL = baseURL
        , tuneMetadata = tuneMetadata
        , tuneResult = tuneResult
        , comments = either (const []) identity comments
        , instruments = instruments
        } )
    HandleTuneIsPlaying (PC.IsPlaying p) -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
      pure unit
    DeleteComment commentId -> do
      state <- H.get
      currentUser <- getUser
      baseURL <- getBaseURL
      case currentUser of
        Just credentials -> do
          result <- deleteComment baseURL state.genre state.tuneId commentId credentials
          case result of
            Right _ -> do
              let
                newState = removeComment commentId state
              H.put newState
            Left _ ->
              pure unit
        Nothing ->
          pure unit


urlPreface :: State -> String
urlPreface state =
  (show state.baseURL)
  <> "/genre/"
  <> (asUriComponent state.genre)
  <> "/tune/"
  <> state.tuneURI

-- | remove the comment from state
removeComment :: CommentId -> State -> State
removeComment cId state =
  let
    newComments = filter (\c -> c.commentId /= cId) state.comments
  in
    state { comments = newComments }
