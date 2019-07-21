module TuneBank.Page.SearchForm where

import Prelude (Unit, Void, ($), (==), (<<<), (>), (/=), bind, identity, pure, map, unit, show)
import Data.Const (Const)
import Data.String.CodePoints (length)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TuneBank.HTML.Header (header)
import TuneBank.HTML.Footer (footer)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams, defaultSearchParams)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Key (keySearchTerm)
import TuneBank.Data.Key as K
import TuneBank.Data.Rhythm as R
import TuneBank.HTML.Utils (css)
import TuneBank.Page.Utils.Environment (getCurrentGenre)

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , searchParams :: SearchParams
  , title :: Maybe String
  , key :: Maybe String
  , rhythm :: Maybe String
  , ordering :: String
  }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Initialize
  | HandleTitle String
  | HandleKey String
  | HandleRhythm String
  | HandleOrdering String
  | Search

defaultOrdering :: String
defaultOrdering =
  "alpha"

defaultOtherMenu :: String
defaultOtherMenu =
  "any"


component
   :: ∀ i o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
   => Navigate m
   => H.Component HH.HTML Query i o m
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

  initialState :: i -> State
  initialState _ =
   { genre : Scandi
   , searchParams : defaultSearchParams
   , title : Nothing
   , key : Nothing
   , rhythm : Nothing
   , ordering : defaultOrdering
   }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ header Nothing SearchForm
      , HH.form
        [ HP.id_ "searchform" ]
        [ HH.fieldset
            []
            [ HH.legend_ [HH.text "Tune Search"]
            , renderTuneName state
            , renderKeyMenu state
            , renderRhythmMenu state
            , renderOrderingMenu state
            , renderSearchButton state
            ]
        ]
      , footer
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      H.modify_ (\state -> state { genre = genre } )
    HandleTitle title -> do
      if (length title > 0)
        then do
          state <- H.get
          let
            searchParams = state.searchParams { title = Just title }
          H.modify_ (\st -> st { searchParams = searchParams } )
        else pure unit
    HandleKey keyName ->
      if (keyName /= defaultOtherMenu)
        then do
          state <- H.get
          let
            searchParams = state.searchParams { key = keySearchTerm keyName }
          H.modify_ (\st -> st { searchParams = searchParams } )
        else pure unit
    HandleRhythm rhythm ->
      if (rhythm /= defaultOtherMenu)
        then do
          state <- H.get
          let
            searchParams = state.searchParams { rhythm = Just rhythm }
          H.modify_ (\st -> st { searchParams = searchParams } )
        else pure unit
    HandleOrdering ordering ->
       H.modify_ (\state -> state { ordering = ordering } )
    Search -> do
      state <- H.get
      navigate $ TuneList state.searchParams

renderTuneName :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderTuneName state =
  HH.div
    [ css "textinput-div" ]
    [ HH.label
      [ css "textinput-label" ]
      [ HH.text "name:" ]
    , HH.input
        [ css "textinput"
        , HE.onValueInput  (Just <<< HandleTitle)
        , HP.value (fromMaybe "" state.title)
        , HP.type_ HP.InputText
        ]
    ]

renderKeyMenu :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderKeyMenu state =
  let
    defaultKey = maybe "any" identity state.key
  in
     HH.div
       [ css "dropdown-div" ]
       [ HH.label
         [ css "dropdown-label" ]
         [ HH.text "key:" ]
         , HH.select
            [ css "dropdown-selection"
            , HP.id_  "key-menu"
            , HP.value (fromMaybe defaultOtherMenu state.key)
            , HE.onValueChange  (Just <<< HandleKey)
            ]
            (keyOptions defaultKey)
        ]

keyOptions :: forall i p. String -> Array (HH.HTML i p)
keyOptions default =
  map (menuOption default) K.keys

renderRhythmMenu :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderRhythmMenu state =
  let
    default = maybe "any" identity state.rhythm
  in
     HH.div
       [ css "dropdown-div" ]
       [ HH.label
         [ css "dropdown-label" ]
         [ HH.text "rhythm:" ]
         , HH.select
            [ css "dropdown-selection"
            , HP.id_  "rhythm-menu"
            , HP.value (fromMaybe defaultOtherMenu state.rhythm)
            , HE.onValueChange  (Just <<< HandleRhythm)
            ]
            (rhythmOptions state.genre default)
        ]

rhythmOptions :: forall i p. Genre -> String -> Array (HH.HTML i p)
rhythmOptions genre default =
  map (menuOption default) (R.rhythms genre)


menuOption :: forall i p. String -> String -> HH.HTML i p
menuOption default next  =
  let
    selected = (next == default)
  in
    HH.option
      [ HP.disabled (selected) ]
      [ HH.text next ]

renderOrderingMenu :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderOrderingMenu state =
  let
    default = "alpha"
  in
     HH.div
       [ css "dropdown-div" ]
       [ HH.label
         [ css "dropdown-label" ]
         [ HH.text "ordering:" ]
         , HH.select
            [ css "dropdown-selection"
            , HP.id_  "ordering-menu"
            , HP.value state.ordering
            , HE.onValueChange  (Just <<< HandleOrdering)
            ]
            (orderingOptions default)
        ]

orderingOptions :: forall i p. String -> Array (HH.HTML i p)
orderingOptions default =
  [ menuOption default "alpha"
  , menuOption default "most recent"
  ]

renderSearchButton :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderSearchButton state =
    HH.button
      [ HE.onClick \_ -> Just Search
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "search" ]
