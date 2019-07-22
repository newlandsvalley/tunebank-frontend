-- | This module exports a pure HTML function to render a consistent header throughout the app.
module TuneBank.HTML.Header where

import Prelude


import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (guard)
import Halogen.HTML as HH
import TuneBank.Data.Credentials (Credentials)
import TuneBank.Data.Genre (Genre)
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Route (Route(..))


header  :: forall i p. Maybe Credentials -> Genre -> Route -> HH.HTML i p
header mCredentials genre route =
  HH.header
    [ css "masthead" ]
    [ HH.div_
      [ HH.nav
        [ css "header-nav" ]
        [ HH.ul
          [ css "nav" ]
          [ navItem Home
             [ HH.text "home" ]
          , navItem Genre
             [ HH.text "genre" ]
          , navItem Upload
             [ HH.text "upload" ]
          , navItem Login
             [ HH.text $ maybe "login" (const "logout") mCredentials ]
          ]
        ]
      , userState
      ]
    ]

  where

  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css $ guard (route == r) "current"
        , safeHref r
        ]
        html
      ]

  userState ::HH.HTML i p
  userState  =
    let
      loginState = maybe "not logged in" (\credentials -> credentials.user) mCredentials
    in
      HH.ul
        [ css "masthead2" ]
        [ HH.li
          [ css "masthead2-genre" ]
          [ HH.text (show genre) ]
        , HH.li
          [ css "masthead2-user" ]
          [ HH.text loginState ]
        ]
