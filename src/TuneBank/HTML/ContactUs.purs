module TuneBank.HTML.ContactUs (contactUs) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

contactUs :: forall i p. HH.HTML i p
contactUs =
  HH.div
    [ HP.id_ "contactusdiv" ]
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
    , HH.p_
      [
        HH.text "The Scandi section has been developed in cooperation with the "
      , HH.a
         [ HP.href "https://www.londonscandisession.co.uk/"]
         [ HH.text "London Scandi Session"]
      , HH.text ". It represents a fair cross-section of tunes that are regularly played at the session."
      ]
    ]
