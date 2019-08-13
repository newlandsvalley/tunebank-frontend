module TuneBank.Page.AdvancedSearchForm where

import Prelude (Unit, Void, ($), (<<<), (<>),  bind, identity, pure, map, unit)
import Data.Const (Const)
import Data.String.CodePoints (length)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.SearchParams (SearchParams, defaultSearchParams)
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
  , searchParams :: SearchParams
  , criteria :: String
  }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Initialize
  | HandleCriteria String
  | Search


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
   , criteria : ""
   }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.form
        [ HP.id_ "advancedsearchform" ]
        [ HH.fieldset
            []
            [ HH.legend_ [HH.text "Advanced Search"]
            , renderAdvisoryText
            , renderCriteria state
            , renderSearchButton state
            ]
        ]
        , renderSearchExamples
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      H.modify_ (\state -> state { genre = genre } )
    HandleCriteria criteria -> do
      {-}
      if (length title > 0)
        then do
          state <- H.get
          let
            searchParams = state.searchParams { title = Just title }
          H.modify_ (\st -> st { searchParams = searchParams } )
        else
      -}
          pure unit
    Search -> do
      state <- H.get
      pure unit
      {-}
      navigate $ TuneList state.searchParams
      -}

renderCriteria :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderCriteria state =
  HH.div
    [ css "textinput-div" ]
    [ HH.label
      [ css "textinput-label" ]
      [ HH.text "criteria:" ]
    , HH.input
        [ css "textinput"
        , HP.id_ "search-criteria"
        , HE.onValueInput  (Just <<< HandleCriteria)
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
renderSearchButton state =
    HH.button
      [ HE.onClick \_ -> Just Search
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "search" ]

renderSearchExamples :: forall m. H.ComponentHTML Action ChildSlots m
renderSearchExamples =
  HH.div
    [ HP.id_ "search-examples-div"]
    [ HH.text "Some example searches:"
    , HH.dl
        [ HP.id_ "search-examples"]
        [ renderKV "title=Sligo" "'sligo' in the tune name"
        , renderKV "rhythm=slip jig" "slip jigs"
        , renderKV "rhythm=jig" "both jigs and slip jigs"
        , renderKV "abc-fedd" "tunes with this succession of notes (irrespective of octave)"
        , renderKV "transcriber=Fred" "tunes transcribed by Fred"
        , renderKV "submitterer=Fred" "tunes submitted by Fred"
        , renderKV "rhythm=reel&key=BMin"	"reels in B Minor"
        ]
    ]
