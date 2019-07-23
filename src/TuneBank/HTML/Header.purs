-- | This module exports a pure HTML function to render a consistent header throughout the app.
module TuneBank.HTML.Header where

import Prelude


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
          , loggedInUserNavItem Upload
             [ HH.text "upload" ]
          , navItem Login
             [ HH.text $ maybe "login" (const "logout") mCredentials ]
          ]
        ]
      , userState
      ]
    ]

  where

  -- | a navigation item available at any time
  navItem :: Route -> Array (HH.HTML i p) -> HH.HTML i p
  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css $ guard (route == r) "current"
        , safeHref r
        ]
        html
      ]

  -- | a navigation item available to any logged-in user
  loggedInUserNavItem :: Route -> Array (HH.HTML i p) -> HH.HTML i p
  loggedInUserNavItem r html =
    maybe (HH.text "") (const $ navItem r html) mCredentials

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
