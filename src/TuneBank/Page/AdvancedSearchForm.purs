module TuneBank.Page.AdvancedSearchForm where

import Prelude (Unit, Void, ($), (<>),  bind, const, pure, unit)
import Data.Const (Const)
import Data.String.Common (null)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams, parseParams)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.Utils (css, renderKV)
import TuneBank.Page.Utils.Environment (getCurrentGenre)
import TuneBank.Data.SearchKeys as Keys

-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre
  , criteria :: String
  , parsedParams :: Either String SearchParams
  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleCriteria String
  | Search MouseEvent


component
   :: ∀ i o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
   => Navigate m
   => H.Component Query i o m
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
   , criteria : ""
   , parsedParams : Left ""
   }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.form
        [ HP.id "advancedsearchform" ]
        [ HH.fieldset
            []
            [ HH.legend_ [HH.text "Advanced Search"]
            , renderAdvisoryText
            , renderCriteria state
            , renderSearchButton state
            ]
        , renderError state
        ]
        , renderSearchExamples
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      H.modify_ (\state -> state { genre = genre } )
    HandleCriteria criteria -> do
      H.modify_ (\st -> st { criteria = criteria, parsedParams = Left "" } )
    Search event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      state <- H.get
      let
        parsedParams = parseParams state.criteria
      _ <- H.modify_ (\st -> st { parsedParams = parsedParams  } )
      case parsedParams of
        Left _ ->
          pure unit
        Right searchParams ->
          navigate $ TuneList searchParams

renderCriteria :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderCriteria state =
  HH.div
    [ css "textinput-div" ]
    [ HH.label
      [ css "textinput-label" ]
      [ HH.text "criteria:" ]
    , HH.input
        [ css "textinput"
        , HP.id "search-criteria"
        , HE.onValueInput  HandleCriteria
        , HP.value state.criteria
        , HP.type_ HP.InputText
        ]
    ]

renderAdvisoryText :: forall m. H.ComponentHTML Action ChildSlots m
renderAdvisoryText =
  let
    text1 =
       "You can use any of the following search terms: "
    text2 =
       Keys.title <> ", " <> Keys.rhythm <> ", " <> Keys.composer <> ", " <>
       Keys.key <> ", " <> Keys.origin <> ", " <> Keys.source <> ", " <>
       Keys.submitter <> ", " <> Keys.transcriber <> ", " <> Keys.abc <> " "
    text3 =
       "and join them together with "
    text4 =
       "&."
  in
    HH.div_
      [ HH.p_
          [ HH.text text1
          , HH.b_ [ HH.text text2 ]
          , HH.text text3
          , HH.b_ [ HH.text text4 ]
          ]
      ]

renderSearchButton :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderSearchButton _state =
    HH.button
      [ HE.onClick Search
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "search" ]

renderSearchExamples :: forall m. H.ComponentHTML Action ChildSlots m
renderSearchExamples =
  HH.div
    [ HP.id "search-examples-div"]
    [ HH.text "Some example searches:"
    , HH.dl
        [ HP.id "search-examples"]
        [ renderKV "title=Sligo" "'sligo' in the tune name"
        , renderKV "rhythm=slip jig" "slip jigs"
        , renderKV "rhythm=jig" "jigs"
        , renderKV "abc=fedd" "tunes with this succession of notes (irrespective of octave)"
        , renderKV "transcriber=Fred" "tunes transcribed by Fred"
        , renderKV "submitterer=Fred" "tunes submitted by Fred"
        , renderKV "rhythm=reel&key=BMin" "reels in B Minor"
        ]
    ]

renderError :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderError state =
  let
    f :: String -> String
    f err =
      if (null err) then "" else "invalid search criteria"
    errorText = either f (const "") state.parsedParams
  in
    HH.div_
      [ HH.b_
        [ HH.text errorText ]
      ]
