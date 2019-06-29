module TuneBank.Navigation.Router where

-- | The Router Halogen Component


import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (class MonadAsk)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TuneBank.Navigation.Route (Route(..), routeCodec)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Page.Login as Login
import TuneBank.Page.SearchForm as SearchForm
import TuneBank.Page.GenreMenu as GenreMenu
import TuneBank.Page.Register as Register
import TuneBank.Page.Upload as Upload
import TuneBank.Page.Tune as Tune
import TuneBank.Page.TuneList as TuneList
import TuneBank.Page.Home as Home
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import Routing.Duplex as RD
import Routing.Hash (getHash)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the component
-- | is by sending input.
type OpaqueSlot = H.Slot (Const Void) Void

type State =
  { route :: Maybe Route }

data Query a
  = Navigate Route a

data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , search :: SearchForm.Slot Unit
  , genre :: GenreMenu.Slot Unit
  , login :: Login.Slot Unit
  , register :: Register.Slot Unit
  , upload :: Upload.Slot Unit
  , tune :: Tune.Slot Unit
  , tunelist :: TuneList.Slot Unit
  )

component ::
    ∀ m r
    . MonadAff m
    => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
    => Navigate m
    => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
      -- and, finally, we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
         H.modify_ _ { route = Just dest }
      pure (Just a)

  -- | Note - links are not well-typed.  Sproxy names must also match the
  -- | child slot names AND the route codec initial URI name.
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home ->
        HH.slot (SProxy :: _ "home") unit Home.component unit absurd
      SearchForm ->
        HH.slot (SProxy :: _ "search") unit SearchForm.component unit absurd
      Genre ->
        HH.slot (SProxy :: _ "genre") unit GenreMenu.component unit absurd
      Login ->
        HH.slot (SProxy :: _ "login") unit Login.component unit absurd
      Register ->
        HH.slot (SProxy :: _ "register") unit Register.component unit absurd
      Upload ->
        HH.slot (SProxy :: _ "upload") unit Upload.component unit absurd
      Tune _ _ ->
        HH.slot (SProxy :: _ "tune") unit Tune.component unit absurd
      TuneList searchParams ->
        HH.slot (SProxy :: _ "tunelist") unit TuneList.component { searchParams } absurd

    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
