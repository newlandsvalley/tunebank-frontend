module TuneBank.HTML.ContactUs (contactUs) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

contactUs :: forall i p. HH.HTML i p
contactUs =
  HH.div
    [ HP.id "contactusdiv" ]
    [ HH.h2_
       [ HH.text "Please get in touch" ]
    , HH.p_
      [
        HH.text "Perhaps you have questions about the site or suggestions for improvements. If so, "
      , HH.a
        [ HP.href "mailto:john.watson@@gmx.co.uk"]
        [ HH.text "contact us"]
      , HH.text " and we'd be delighted to hear from you."
      ]
    ]
