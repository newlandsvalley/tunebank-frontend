module TuneBank.Navigation.Router where

-- | The Router Halogen Component


import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import TuneBank.Page.AdvancedSearchForm as AdvancedSearchForm
import TuneBank.Page.GenreMenu as GenreMenu
import TuneBank.Page.Register as Register
import TuneBank.Page.Upload as Upload
import TuneBank.Page.UserList as UserList
import TuneBank.Page.Tune as Tune
import TuneBank.Page.TuneList as TuneList
import TuneBank.Page.Comment as Comment
import TuneBank.Data.CommentId (commentKey)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Credentials (Credentials)
import TuneBank.HTML.Footer (footer)
import TuneBank.HTML.Header (header)
import TuneBank.Page.Utils.Environment (getUser, getCurrentGenre)
import TuneBank.HTML.About (about)
import TuneBank.HTML.Credits (credits)
import TuneBank.HTML.Help (help)
import Routing.Duplex as RD
import Routing.Hash (getHash)


import Debug.Trace (spy, trace, traceM)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the component
-- | is by sending input.
type OpaqueSlot = H.Slot (Const Void) Void

type State =
  { route :: Maybe Route
  , genre :: Genre
  , currentUser :: Maybe Credentials
  }

data Query a
  = Navigate Route a

data Action
  = Initialize

type ChildSlots =
  ( home ::  SearchForm.Slot  Unit
  , genre :: GenreMenu.Slot Unit
  , login :: Login.Slot Unit
  , register :: Register.Slot Unit
  , upload :: Upload.Slot Unit
  , advancedsearch :: AdvancedSearchForm.Slot Unit
  , userlist :: UserList.Slot Unit
  , tune :: Tune.Slot Unit
  , tunelist :: TuneList.Slot Unit
  , comment :: Comment.Slot Unit
  )

component ::
    ∀ m r
    . MonadAff m
    => MonadAsk { session :: Session, baseURL :: BaseURL | r } m
    => Navigate m
    => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing
                          , genre : Scandi
                          , currentUser : Nothing }
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
      state <- H.get
      user <- getUser
      genre <- getCurrentGenre
      -- don't re-render unnecessarily if the state is unchanged
      when ((state.route /= Just dest)
           || (state.genre /= genre)
           || (state.currentUser /= user)
           ) do
         H.modify_ _ { route = Just dest, genre = genre, currentUser = user }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = do
    let
      route = maybe Home identity state.route
    HH.div_
        [  header state.currentUser state.genre route
        ,  renderRoute state
        ,  footer
        ]

  -- | Note - links are not well-typed.  Sproxy names must also match the
  -- | child slot names AND the route codec initial URI name.
  renderRoute :: State -> H.ComponentHTML Action ChildSlots m
  renderRoute { route } =
    let
      foo = spy "rendering route: " $ show route
    in
      case route of
        Just r -> case r of
          Home ->
            HH.slot (SProxy :: _ "home") unit SearchForm.component unit absurd
          Genre ->
            HH.slot (SProxy :: _ "genre") unit GenreMenu.component unit absurd
          Login ->
            HH.slot (SProxy :: _ "login") unit Login.component unit absurd
          Register ->
            HH.slot (SProxy :: _ "register") unit Register.component unit absurd
          Upload ->
            HH.slot (SProxy :: _ "upload") unit Upload.component unit absurd
          AdvancedSearch ->
            HH.slot (SProxy :: _ "advancedsearch") unit AdvancedSearchForm.component unit absurd
          UserList pageParams  ->
            HH.slot (SProxy :: _ "userlist") unit UserList.component pageParams absurd
          Tune genre tuneId  ->
            HH.slot (SProxy :: _ "tune") unit Tune.component { genre, tuneId } absurd
          TuneList searchParams ->
            HH.slot (SProxy :: _ "tunelist") unit TuneList.component { searchParams } absurd
          Comments genre tuneId  ->
            HH.slot (SProxy :: _ "comment") unit Comment.component { genre, tuneId, key : Nothing } absurd
          Comment genre tuneId user cid ->
            HH.slot (SProxy :: _ "comment") unit Comment.component { genre, tuneId, key : (Just $ commentKey user cid) } absurd
          About ->
            about
          Credits ->
            credits
          Help ->
            help

        Nothing ->
          HH.div_ [ HH.text "Oh no! That page wasn't found." ]
