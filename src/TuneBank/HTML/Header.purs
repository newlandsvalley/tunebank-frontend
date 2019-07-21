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
           [ HH.text $ maybe "login" (\credentials -> ("logout " <> credentials.user)) mCredentials ]
        ]
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
