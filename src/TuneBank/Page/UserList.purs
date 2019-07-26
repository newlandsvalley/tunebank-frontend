module TuneBank.Page.UserList where

import Control.Monad.Reader (class MonadAsk)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Prelude (Unit, Void, ($), (<>), bind, discard, map, pure, show, unit)
import TuneBank.Api.Codec.Pagination (Pagination)
import TuneBank.Api.Codec.UsersPage (UsersPage, UserRef)
import TuneBank.Api.Request (requestUsers)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL)
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Endpoint (PageParams)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getUser)
import TuneBank.HTML.PaginationRendering  (renderPagination)


-- type Slot = H.Slot Query Void
type Slot = H.Slot Query Void

type State =
  { currentUser :: Maybe Credentials
  , pageParams :: PageParams
  , usersResult :: Either String (Tuple UsersPage Pagination)
  }

data Query a =
  FetchResults a

type ChildSlots = ()

data Action
  = Initialize
  | GoToPage Int

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
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
    { currentUser : Nothing
    , pageParams : { page: 1 }
    , usersResult : Left "Not started" }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    case state.usersResult of
      Left err ->
        HH.text err
      Right (Tuple usersPage pagination) ->
        case (length usersPage.users) of
          0 ->
             HH.text "no users found"
          _ ->
            HH.div_
              [
              HH.h4
                 [ css "center" ]
                 [HH.text ("page "
                           <> show state.pageParams.page
                           <> " of "
                           <> show pagination.maxPages
                           )
                 ]
              , renderUserList state usersPage.users
              , renderPagination (UserList state.pageParams) pagination
              ]

  renderUserList :: State -> Array UserRef -> H.ComponentHTML Action ChildSlots m
  renderUserList state users =
    let
      -- f :: forall w i. TuneRef -> HH.HTML w i
      f userRef =
          tableRow userRef.name userRef.email
    in
      HH.table_ $
        map f users
    where

      tableRow name email =
          HH.tr
            []
            [ HH.td
              []
              [ HH.text name]
            , HH.td
              []
              [ HH.text email]
            ]
{-}
  renderPagination :: Route -> Pagination -> H.ComponentHTML Action ChildSlots m
  renderPagination route pagination =
    HH.text "pagination here"
-}


  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      mUser <- getUser
      _ <- H.modify (\state -> state { currentUser = mUser } )
      _ <- handleQuery (FetchResults unit)
      pure unit
    GoToPage page -> do
      _ <- H.modify (\st -> st { pageParams = { page : page} } )
      _ <- navigate $ UserList { page : page }
      _ <- handleQuery (FetchResults unit)
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    FetchResults next -> do
      state <- H.get
      -- live server testing only baseURL <- getCorsBaseURL
      baseURL <- getBaseURL
      usersResult <- requestUsers baseURL state.currentUser state.pageParams
      H.modify_ (\st -> st { usersResult = usersResult } )
      pure (Just next)
