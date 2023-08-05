-- | This module exports a pure HTML function to render a consistent header throughout the app.
module TuneBank.HTML.Header where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes.Color (Color(..))
import TuneBank.Data.Credentials (Credentials, Role(..))
import TuneBank.Data.Genre (Genre)
import TuneBank.HTML.Utils (css, safeHref, svgcss)
import TuneBank.Navigation.Route (Route(..))
import TuneBank.Navigation.RouterTypes (Action(..))

-- | CSS selector for menu items in large screen displays
bigScreenSelector :: String 
bigScreenSelector = "navitem"

-- | CSS selector for menu items in small screen displays (mobiles)
smallScreenSelector :: String 
smallScreenSelector = "mobnavitem"

header  :: forall m childSlots. Maybe Credentials -> Genre -> Route -> H.ComponentHTML Action childSlots m
header mCredentials genre route =
    HH.div_
      [ HH.nav
        [ css "nav" ]
        [ HH.ul
          [ css "main-menu" ]
          [ HH.div
            -- main menu left-hand side options
            [ css "navdiv"]
            [ navItem Home
               [ HH.text "home" ]
            , navGenre bigScreenSelector
            , loggedInUserNavItem Upload
               [ HH.text "upload" ]
            , adminUserNavItem (UserList { page: 1 })
               [ HH.text "users" ]
            , navItem Metronome
               [ HH.text "metronome" ]
            , navItem (Editor { initialAbc : Nothing })
               [ HH.text "abc editor" ]
            ]
          , HH.div 
            -- main menu right-hand side options
            [ css "navdiv"]
            [ navRegister bigScreenSelector
            , navLogInOut bigScreenSelector
            ]
          ]
          -- hamburger navigation - small screens only
        , HH.ul 
          [ css "hamburger-nav"]
          -- hamburger app name
          [ HH.li
             [ css "hamburger-appname"]
             [ HH.text "tradtunedb"]
             -- hamburger icon
          , HH.li 
            [ css "hamburger-icon"] 
            [ HH.div 
              [ css "hamburger-btn"
              , HP.id "hamburger-btn" 
              ]
              [ HH.div
                -- the hamburger lines indicate that the menu can be switched on
                [ HE.onClick \_ -> ToggleHamburgerMenu ]
                [ SE.svg 
                  [ SA.viewBox 0.0 0.0 80.0 80.0 
                  , SA.width 40.0 
                  , SA.height 20.0
                  , svgcss "visible"
                  , SA.id "hamburger-lines"
                  ]
                  [ SE.rect
                    [ SA.width 80.0 
                    , SA.height 10.0
                    , SA.fill white
                    ]
                  , SE.rect
                    [ SA.width 80.0 
                    , SA.height 10.0
                    , SA.fill white
                    , SA.y 30.0
                    ]
                  , SE.rect
                    [ SA.width 80.0 
                    , SA.height 10.0
                    , SA.fill white
                    , SA.y 60.0
                    ]
                  ]
                ]                  
                , HH.div
                -- the hamburger cross indicate that the menu can be switched off
                  [ HE.onClick \_ -> ToggleHamburgerMenu]
                  [ SE.svg 
                    [ SA.viewBox 0.0 0.0 100.0 100.0 
                    , SA.width 40.0 
                    , SA.height 20.0
                    , svgcss "hidden"
                    , SA.id "hamburger-cross"
                    ]                      
                    [ SE.line
                      [ SA.x1 10.0
                      , SA.x2 80.0
                      , SA.y1 10.0
                      , SA.y2 80.0
                      , SA.stroke white
                      , SA.strokeWidth 10.0
                      ]
                    , SE.line
                      [ SA.x1 80.0 
                      , SA.x2 10.0
                      , SA.y1 10.0
                      , SA.y2 80.0
                      , SA.stroke white
                      , SA.strokeWidth 10.0
                      ]
                    ]
                  ]
                ]
            ]
          ]
        ]

        -- hamburger menu pops up when selected by the icom and disappears when deselected
        , HH.div 
          [ css "hamburger-menu"
          , HP.id "hamburger-menu"
          ]
          [ HH.ul
            []
            [ mobnavItem Home  [ HH.text "home" ]
            , mobnavItem Login [ HH.text "login" ]
            , mobnavItem Register  [ HH.text "register" ]
            ]
          ]
      ]

  where

  -- | a navigation item available at any time in the main (large screen) menu
  navItem :: Route -> Array (H.ComponentHTML Action childSlots m) -> H.ComponentHTML Action childSlots m
  navItem r html =
    item r bigScreenSelector html

  -- | a navigation item available in the mobile hamburger menu
  mobnavItem :: Route -> Array (H.ComponentHTML Action childSlots m) -> H.ComponentHTML Action childSlots m
  mobnavItem r html =
    item r smallScreenSelector html
 
  item :: Route -> String -> Array (H.ComponentHTML Action childSlots m) -> H.ComponentHTML Action childSlots m
  item r cssSelector html = 
    HH.li
      [ css cssSelector ]
      [ HH.a
        [ css $ guard (route == r) "current"
        , safeHref r
        ]
        html
      ]

  -- | a navigation item available to any logged-in user
  loggedInUserNavItem :: Route -> Array (H.ComponentHTML Action childSlots m) -> H.ComponentHTML Action childSlots m
  loggedInUserNavItem r html =
    maybe (HH.text "") (const $ navItem r html) mCredentials

  -- | a navigation item available to an admin user
  adminUserNavItem :: Route -> Array (H.ComponentHTML Action childSlots m) -> H.ComponentHTML Action childSlots m
  adminUserNavItem r html =
    let
      isAdmin :: Credentials -> H.ComponentHTML Action childSlots m
      isAdmin cred =
        if (cred.role == Administrator) then
          navItem r html
        else
          HH.text ""
    in
      maybe (HH.text "") isAdmin mCredentials


  -- | a 'special' navigation item for the Genre where we display the current genre in the masthead
  navGenre :: String -> H.ComponentHTML Action childSlots m
  navGenre cssSelector =
    HH.li
      [ css cssSelector ]
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
  navRegister :: String -> H.ComponentHTML Action childSlots m
  navRegister cssSelector =
    case mCredentials of
      Just _credentials ->
        HH.li
         [ css cssSelector ]
         [ HH.div_           
           [ HH.text "" ]]
      Nothing ->
         HH.li
          [ css cssSelector ]
          [ HH.a
            [ css $ guard (route == Register) "current"
            , safeHref Register
            ]
            [ HH.text "register" ]
          ]

  -- | a 'special' navigation item for Login/Logout
  -- | which is situated on the right of the menu bar after Register
  navLogInOut :: String -> H.ComponentHTML Action childSlots m
  navLogInOut cssSelector =
    case mCredentials of
      Just credentials ->
        HH.li
         [ css cssSelector ]
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
          [ css cssSelector ]
          [ HH.a
            [ css $ guard (route == Login) "current"
            , safeHref Login
            ]
            [ HH.text "login" ]
          ]




white :: Color
white = 
  RGB 255 255 255
      