module TuneBank.Page.GenreMenu where

import Prelude (Unit, Void, ($), (==), (<>), bind, map, pure, unit, show)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Enum (enumFromTo)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk, asks)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Data.Genre (Genre(..), readGenre)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.Utils (css)
import TuneBank.Page.Utils.Environment (getCurrentGenre)


type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre  }

type Query :: forall k. k -> Type
type Query = (Const Void)

type ChildSlots :: forall k. Row k
type ChildSlots = ()

data Action
  = Initialize
  | HandleGenre String

component
   :: ∀ i o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL  | r } m
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
    { genre : Scandi  }


  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.form
        [ HP.id "genreform" ]
        [ HH.fieldset
            []
            [ HH.legend_ [HH.text "Genre"]
            , renderGenreMenu state
            , renderAdvisoryText state
            ]
        ]
      ]

  renderAdvisoryText :: State -> H.ComponentHTML Action ChildSlots m
  renderAdvisoryText _state =
    let
      text =
         "Choose the genre to be used in all tune searches, " <>
          "uploads etc."
    in
      HH.div_
        [ HH.p_
            [HH.text text]
        ]


  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      H.modify_ (\state -> state { genre = genre } )
    HandleGenre genreString ->
      case (readGenre genreString) of
        Nothing ->
          pure unit
        Just genre -> do
          -- save locally
          _ <- H.modify (\state -> state { genre = genre } )
          -- save globally - in the reference in the session
          session <- asks _.session
          _ <- H.liftEffect $ Ref.write genre session.genre
          -- we MUST navigate in order to update the headers
          navigate Home


renderGenreMenu :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderGenreMenu state =
     HH.div
       [ css "nav-menu" ]
       [ HH.label
         [ css "nav-menu-label" ]
         [ HH.text "select:" ]
         , HH.select
            [ css "nav-selection"
            , HP.id  "genre-menu"
            , HP.value (show state.genre)
            , HE.onValueChange  HandleGenre
            ]
            (genreOptions state.genre)
        ]

genreOptions :: forall i p. Genre -> Array (HH.HTML i p)
genreOptions default =
  map (\o -> genreOption o default) $ enumFromTo English Scottish

genreOption :: forall i p. Genre -> Genre -> HH.HTML i p
genreOption next default =
  let
    selected = (next == default)
  in
    HH.option
      [ HP.disabled (selected) ]
      [ HH.text (show next)]
