module TuneBank.Page.Upload where

import Control.Monad.Reader (class MonadAsk)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.MediaType (MediaType(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.FileInputComponent as FIC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<<<), (<>), bind, discard, pure, show, unit)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre, getUser)
import TuneBank.Api.Request (postTune)

-- type Slot = H.Slot Query Void
type Slot = H.Slot Query Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , baseURL :: BaseURL
  , fileName :: Maybe String
  , abc :: Maybe String
  , postResult :: Either String String
  }

data Query a =
  PostTune a

type ChildSlots =
   ( abcfile :: FIC.Slot Unit )


_abcfile = SProxy :: SProxy "abcfile"

abcFileInputCtx :: FIC.Context
abcFileInputCtx =
  { componentId : "abcinput"
  , isBinary : false
  , prompt : "choose file"
  , accept :  mediaType (MediaType ".abc")
  }

data Action
  = Initialize
  | HandleABCFile FIC.Message
  | UploadFile

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
    , currentUser : Nothing
    , baseURL : BaseURL ""
    , fileName : Nothing
    , abc : Nothing
    , postResult : Left ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.form
        [ HP.id_ "uploadform" ]
        [ HH.fieldset
            []
            [ HH.legend_ [HH.text "Upload Tune"]
            , renderAdvisoryText state
            , renderSelectFile state
            , renderUploadButton state
            ]
        ]
      ]
  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      baseURL <- getBaseURL
      mUser <- getUser
      genre <- getCurrentGenre
      _ <- H.modify (\state -> state { genre = genre
                                     , currentUser = mUser
                                     , baseURL = baseURL } )
      pure unit
    HandleABCFile (FIC.FileLoaded filespec) -> do
      _ <- H.modify (\st -> st { fileName = Just filespec.name
                                ,  abc = Just filespec.contents} )
      pure unit
    UploadFile -> do
      _ <- handleQuery (PostTune unit)
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    PostTune next -> do
      state <- H.get
      baseURL <- getBaseURL
      case (Tuple state.currentUser state.abc) of
        (Tuple (Just credentials) (Just abc) ) -> do
          postResult <- postTune abc baseURL state.genre credentials
          H.modify_ (\st -> st { postResult = postResult } )
          pure (Just next)
        _ ->
          pure (Just next)

renderAdvisoryText :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderAdvisoryText state =
  let
    text1 =
       "Upload an ABC file to the " <>
        (show state.genre) <>
         " genre. (The file extension should be "  <>
         ".txt although some browsers support .abc)."
    text2 =
       "The file should contain a single tune."
  in
    HH.div_
      [ HH.p_
          [HH.text text1]
      , HH.p_
          [HH.text text2]
      ]

renderSelectFile :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
renderSelectFile state =
  HH.div
    [ css "fileinput-div"
    , HP.id_ "uploadselectfile"
    ]
    [ HH.label
      [ css "fileinput-label" ]
      [ HH.text "load ABC file:" ]
    , HH.slot _abcfile unit (FIC.component abcFileInputCtx) unit (Just <<< HandleABCFile)
    ]

renderUploadButton :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderUploadButton state =
    HH.button
      [ HE.onClick \_ -> Just UploadFile
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "upload" ]
