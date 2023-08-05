module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (singleton)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.HTML.HTMLElement (toNode)
import Web.DOM.Node (setTextContent)
import Web.Storage.Storage (getItem)
import TuneBank.Navigation.Route (routeCodec)
import TuneBank.Navigation.Router as Router
import TuneBank.Navigation.RouterTypes (Input) as RouterTypes
import TuneBank.Data.Types (BaseURL(..), LogLevel(..), Env)
import TuneBank.Data.Genre (Genre(..))
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Audio.SoundFont (loadPianoSoundFont)
import AppM (runAppM)


-- | production Musicrest server
-- |  baseURL = BaseURL "http://www.tradtunedb.org.uk:8080/musicrest"
-- | dev server
-- |  baseURL = BaseURL "http://192.168.0.113:8080/musicrest"
productionServer :: String
productionServer =
   "http://www.tradtunedb.org.uk:8080/musicrest"

-- | we pass parameters to the application by means of local storage.
-- | if there is no baseURL item, then we automatically fall back
-- | to the production server
getBaseURL :: Effect BaseURL
getBaseURL = do
  w <- window
  s <- localStorage w
  base <- getItem "baseURL" s
  pure $ BaseURL $ fromMaybe productionServer base

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    bodyNode = toNode body
    
  -- loading the piano soundfont may take time - issue a warning
  liftEffect $ setTextContent "Loading piano soundfont - please wait" bodyNode
  instrument <- loadPianoSoundFont "assets/soundfonts"
  liftEffect $ setTextContent "" bodyNode

  user <- liftEffect $ Ref.new Nothing
  genre <- liftEffect $ Ref.new Scandi
  instruments <- liftEffect $ Ref.new (singleton instrument)
  baseURL <- liftEffect $ getBaseURL

  -- Halogen only deals in Aff at the top level. We have to hoist our monad
  -- (which only adds Navigation to Aff) into Aff so Halogen can deal with it
  let
    session = { user, genre, instruments }
    logLevel = Dev
    -- the basic environment
    environment :: Env
    environment = { session, baseURL, logLevel }


    rootComponent :: H.Component Router.Query RouterTypes.Input Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

  halogenIO <- runUI rootComponent { instruments : singleton instrument } body

  -- The app is running. All that's left is to notify the router
  -- any time the location changes in the URL.
  --
  -- We're using hash-based routing, so we'll use the `matchesWith` function from `Routing.Hash` to
  -- listen for changes in the hash and parse the result (using our routing codec, `routeCodec`,
  -- along with the `parse` function from `Routing.Duplex`). Any time we parse a new location we'll
  -- trigger a `Navigate` query in the router.
  --
  -- See:
  --
  -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
  -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ do 
        _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
        pure unit

