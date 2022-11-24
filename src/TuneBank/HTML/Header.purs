-- | This module exports a pure HTML function to render a consistent header throughout the app.
module TuneBank.HTML.Header where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Halogen.HTML as HH
import TuneBank.Data.Credentials (Credentials, Role(..))
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
          , navGenre
          , loggedInUserNavItem Upload
             [ HH.text "upload" ]
          , adminUserNavItem (UserList { page: 1 })
             [ HH.text "users" ]
          , navItem Metronome
             [ HH.text "metronome" ]
          , navItem (Editor { initialAbc : Nothing })
             [ HH.text "abc editor" ]
          , HH.div 
             [ css "nav-menu-right"]
             [ navRegister
             , navLogInOut
             ]
          ]
        ]
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

  -- | a navigation item available to an admin user
  adminUserNavItem :: Route -> Array (HH.HTML i p) -> HH.HTML i p
  adminUserNavItem r html =
    let
      isAdmin :: Credentials -> HH.HTML i p
      isAdmin cred =
        if (cred.role == Administrator) then
          navItem r html
        else
          HH.text ""
    in
      maybe (HH.text "") isAdmin mCredentials


  -- | a 'special' navigation item for the Genre
  navGenre :: HH.HTML i p
  navGenre =
    HH.li
      [ css "nav-item" ]
      [ HH.a
        [ css $ guard (route == Genre) "current"
        , safeHref Genre
        ]
        [ HH.text "genre" ]
      , HH.span
        [ css "additional-info" ]
        [ HH.text $ show genre ]
      ]

  -- | a navigation item for Register which appears if there's no login
  -- | and is situated on the right of the menu bar before the Login
  navRegister :: HH.HTML i p
  navRegister  =
    case mCredentials of
      Just _credentials ->
        HH.li
         [ css "nav-item" ]
         [ HH.div_           
           [ HH.text "" ]]
      Nothing ->
         HH.li
          [ css "nav-item" ]
          [ HH.a
            [ css $ guard (route == Register) "current"
            , safeHref Register
            ]
            [ HH.text "register" ]
          ]

  -- | a 'special' navigation item for Login/Logout
  -- | which is situated on the right of the menu bar after Register
  navLogInOut :: HH.HTML i p
  navLogInOut  =
    case mCredentials of
      Just credentials ->
        HH.li
         [ css "nav-item" ]
         [ HH.a
           [ css $ guard (route == Login) "current"
           , safeHref Login
           ]
           [ HH.text "logout" ]
         , HH.span
           [ css "additional-info" ]
           [ HH.text credentials.user ]
         ]
      Nothing ->
         HH.li
          [ css "nav-item" ]
          [ HH.a
            [ css $ guard (route == Login) "current"
            , safeHref Login
            ]
            [ HH.text "login" ]
          ]

