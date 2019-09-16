module TuneBank.Page.Upload where

import Control.Monad.Reader (class MonadAsk)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Abc.Metadata (getTitle)
import Data.Abc.Parser (parse)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Halogen as H
import Halogen.FileInputComponent as FIC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, ($), (<<<), (<>), bind, const, discard, identity, pure, show, unit)
import TuneBank.Api.Request (postTune)
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre(..))
import TuneBank.Data.Session (Session)
import TuneBank.Data.TuneId (tuneIdFromString)
import TuneBank.Data.Types (BaseURL(..))
import TuneBank.HTML.Utils (css)
import TuneBank.Navigation.Navigate (class Navigate, navigate)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Page.Utils.Environment (getBaseURL, getCurrentGenre, getUser)

-- type Slot = H.Slot Query Void
type Slot = H.Slot Query Void

type State =
  { genre :: Genre
  , currentUser :: Maybe Credentials
  , baseURL :: BaseURL
  , fileName :: Maybe String
  , abc :: String
  , errorText :: String
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
  | UploadFile MouseEvent

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
    , abc : ""
    , errorText : ""
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
        , renderError state
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
                                ,  abc = filespec.contents
                                , errorText = "" } )
      pure unit
    UploadFile event -> do
      _ <- H.liftEffect $ preventDefault $ toEvent event
      _ <- handleQuery (PostTune unit)
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    PostTune next -> do
      state <- H.get
      baseURL <- getBaseURL
      -- basic validation - the ABC tune parses and has a title
      let
        eTuneTitle = getTuneTitle state.abc
      case (Tuple state.currentUser eTuneTitle) of
        (Tuple (Just credentials) (Right title) ) -> do
          postResult <- postTune state.abc baseURL state.genre credentials
          let
            errorText = either identity (const "") postResult
          H.modify_ (\st -> st { errorText = errorText } )
          case postResult of
            -- we posted OK and got a good response
            Right tuneIdStr -> do
              let
                eTuneId = tuneIdFromString tuneIdStr
              case eTuneId of
                -- the response is not a valid tuneIf - shouldn;t happen -
                -- just navigate home
                Left _ -> do
                  _ <- navigate Home
                  pure (Just next)
                -- we got a valid tuneId so navigate to that tune
                Right tuneId -> do
                  _ <- navigate $ Tune state.genre tuneId
                  pure (Just next)
            Left _ ->
              pure (Just next)
        (Tuple _ (Left err) ) -> do
          H.modify_ (\st -> st { errorText = err } )
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
      [ HE.onClick (Just <<< UploadFile)
      , css "hoverable"
      , HP.enabled true
      ]
      [ HH.text "upload" ]

renderError :: forall m. State -> H.ComponentHTML Action ChildSlots m
renderError state =
  HH.div
    []
    [ HH.text state.errorText ]

getTuneTitle :: String -> Either String String
getTuneTitle abc =
  case parse (abc <> "\r\n")  of
    Left err -> Left ("invalid ABC: " <> show err)
    Right tune ->
      maybe (Left "No tune title present") (\t -> Right t) $ getTitle tune
