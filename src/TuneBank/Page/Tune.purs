module TuneBank.Page.Tune where

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Array (length)
import Data.Either (Either(..), either)
import Data.Bifunctor (lmap)
import Data.Symbol (SProxy(..))
import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<>), (<<<), (>>=), bind, identity, pure, show, unit)
import TuneBank.Navigation.Navigate (class Navigate)
import TuneBank.Data.Genre (Genre(..), asUriComponent)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.Data.TuneId (TuneId(..), encodeTuneIdURIComponent)
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre, getInstruments)
import TuneBank.Api.Codec.Tune (TuneMetadata, nullTuneMetadata)
import TuneBank.Api.Request (requestTune)
import Audio.SoundFont (Instrument)
import Audio.SoundFont.Melody.Class (MidiRecording(..))
import Data.Abc (AbcTune)
import Data.Abc.Parser (parse)
import Data.Abc.Midi (toMidi)

import Halogen.PlayerComponent as PC


currentUser = Nothing

-- | there is no tune yet
nullParsedTune :: Either String AbcTune
nullParsedTune =
  Left "no tune yet"

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , tuneURI :: String
  , tuneId :: TuneId
  , baseURL :: BaseURL
  , tuneMetadata :: TuneMetadata
  , tuneResult :: Either String AbcTune
  , instruments :: Array Instrument
  }

type Input =
  {
    tuneId :: TuneId
  }

type Query = (Const Void)

type ChildSlots =
   (player :: (PC.Slot MidiRecording) Unit)

_player = SProxy :: SProxy "player"

data Action
  = Initialize
  | HandleTuneIsPlaying PC.Message

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
    { genre : Scandi
    , tuneURI : encodeTuneIdURIComponent input.tuneId
    , tuneId : input.tuneId
    , baseURL : BaseURL ""
    , tuneMetadata : nullTuneMetadata
    , tuneResult: nullParsedTune
    , instruments : []
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    let
      (TuneId {title, tuneType}) = state.tuneId
    in
      HH.div_
        [ header Nothing Home
        , HH.h1
           [HP.class_ (H.ClassName "center") ]
           [HH.text ("Tune " <> title) ]
        , renderTuneScore state title
        , renderPlayer state
        , renderDebug state
        , footer
        ]

  renderTuneScore :: State -> String -> H.ComponentHTML Action ChildSlots m
  renderTuneScore state title =
    let
      imageURI = (show state.baseURL) <> "/genre/" <> (asUriComponent state.genre) <>
                 "/tune/" <> state.tuneURI <> "/png"
    in
      HH.div_
        [HH.img
          [ HP.src imageURI
          , HP.alt title
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

  renderDebug ::  State -> H.ComponentHTML Action ChildSlots m
  renderDebug state =
    let
      instrumentCount = length state.instruments
      tuneResult = either identity (\_ -> "tune OK") state.tuneResult
    in
      HH.div_
        [
          HH.text ("instrument count: " <> show instrumentCount)
        , HH.text (" tune result: " <> tuneResult)
        ]



  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      genre <- getCurrentGenre
      baseURL <- getBaseURL
      -- corsBaseURL <- getCorsBaseURL only temporary for live server
      instruments <- getInstruments
      -- tuneMetadataResult <- requestCleanTune baseURL (asUriComponent genre) state.tuneId
      tuneMetadataResult <- requestTune baseURL (asUriComponent genre) state.tuneId
      let
        tuneResult =
          tuneMetadataResult >>= (\x -> lmap show $ parse x.abc)
      H.modify_ (\st -> st
        { genre = genre
        , baseURL = baseURL
        --, tuneMetadata = tuneMetadata
        , tuneResult = tuneResult
        , instruments = instruments
        } )
    HandleTuneIsPlaying (PC.IsPlaying p) -> do
      -- we ignore this message, but if we wanted to we could
      -- disable any button that can alter the editor contents whilst the player
      -- is playing and re-enable when it stops playing
      pure unit
