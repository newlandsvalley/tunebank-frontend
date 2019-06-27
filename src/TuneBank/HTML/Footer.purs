module TuneBank.HTML.Footer (footer) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TuneBank.HTML.Utils (css)


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
           [ HH.a
             [ css "logo-font"
             , HP.href "/"
             ]
             [ HH.text "credits" ]
           ]
         ]
      ]
    ]
