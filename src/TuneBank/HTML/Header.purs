-- | This module exports a pure HTML function to render a consistent header throughout the app.
module TuneBank.HTML.Header where

import Prelude

import TuneBank.Navigation.Route (Route(..))
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Data.Credentials (Credentials)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


header :: forall i p. Maybe Credentials -> Route -> HH.HTML i p
header mCredentials route =
  HH.header
    [ css "masthead" ]
    [ HH.div_
      [ HH.nav
        [ css "header-nav" ]
        [ HH.ul
          [ css "nav" ]
          [ navItem Home
             [ HH.text "home" ]
          , navItem SearchForm
             [ HH.text "search" ]
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
          [ css "masthead2-user" ]
          [ HH.text loginState ]
        ]
