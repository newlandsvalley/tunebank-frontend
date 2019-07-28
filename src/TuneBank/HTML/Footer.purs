module TuneBank.HTML.Footer (footer) where

import Prelude (($), (==))
import Halogen.HTML as HH
import TuneBank.HTML.Utils (css, safeHref)
import TuneBank.Navigation.Route (Route(..))


footer :: forall i p. HH.HTML i p
footer =
  HH.footer
    [ css "footer" ]
    [ HH.h1_
      [ HH.div
        [ css "mainlogo" ]
        [ HH.span
          [ css "mainlogo-prefix" ]
          [ HH.text "tradtunedb" ]
        , HH.span
          [ css "mainlogo-suffix" ]
          [ HH.text ".org.uk" ]
        ]
      , HH.div_
         [ HH.nav
           [ css "footer-nav"]
           [ navItem Credits
              [ HH.text "credits" ]
           ]
         , HH.nav
           [ css "footer-nav"]
           [ navItem Help
              [ HH.text "help" ]              
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
          [ safeHref r
          ]
          html
        ]
