module TuneBank.Page.GenreMenu where

import Prelude (Unit, Void, ($), (==), (<<<), bind, pure, unit, show)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk, asks)
import TuneBank.Navigation.Navigate (class Navigate)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import TuneBank.HTML.Header (header)
import TuneBank.HTML.Footer (footer)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Data.Genre (Genre(..), readGenre)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Page.Utils.Environment (getCurrentGenre)


-- type Slot = H.Slot Query Void
type Slot = H.Slot (Const Void) Void

type State =
  { genre :: Genre }

type Query = (Const Void)

type ChildSlots = ()

data Action
  = Initialize
  | HandleGenre String

component
   :: ∀ i o m r
    . MonadAff m
   => MonadAsk { session :: Session, baseURL :: BaseURL  | r } m
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
    { genre : Scandi }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ header Nothing Home
      , HH.h1
         [HP.class_ (H.ClassName "center") ]
         [HH.text "Genre" ]
      , renderGenreMenu state
      , footer
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      genre <- getCurrentGenre
      _ <- H.modify (\state -> state { genre = genre } )
      pure unit
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
          pure unit


renderGenreMenu :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderGenreMenu state =
     HH.div
       [ css "nav-menu" ]
       [ HH.label
         [ css "nav-menu-label" ]
         [ HH.text "genre:" ]
         , HH.select
            [ css "nav-selection"
            , HP.id_  "genre-menu"
            , HP.value (show state.genre)
            , HE.onValueChange  (Just <<< HandleGenre)
            ]
            (genreOptions state.genre)
        ]

genreOptions :: forall i p. Genre -> Array (HH.HTML i p)
genreOptions default =
  [ genreOption Irish default
  , genreOption Klezmer default
  , genreOption Scandi default
  , genreOption Scottish default
  ]

genreOption :: forall i p. Genre -> Genre -> HH.HTML i p
genreOption next default =
  let
    selected = (next == default)
  in
    HH.option
      [ HP.disabled (selected) ]
      [ HH.text (show next)]
