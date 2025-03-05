module TuneBank.HTML.About (about) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (($), (<>))

about :: forall i p. HH.HTML i p
about =
  HH.div
    [ HP.id "aboutdiv" ]
    [ HH.h2_
        [ HH.text "TradTuneDb version 2.0.3" ]
    , HH.p_
       [ HH.text $
         ("TradTuneDb is a database of English, Irish, Scottish, Scandinavian and klezmer tunes. You can search for tunes and then view the scores " <>
          "or hear them played.  Of course, having them played on a rather tinny midi piano is no substitute for hearing the real thing, " <>
          "but it may help you to learn the basics of a tune because you can vary the tempo. Currently, the player works with the latest " <>
          " releases of all the major browsers other than Internet Explorer. The best quality sound output is to be found with Chrome")
       ]
     , HH.p_
       [
         HH.text "The Scandi section has been developed in cooperation with the "
       , HH.a
          [ HP.href "https://www.facebook.com/londonscandisession/"]
          [ HH.text "London Scandi Session"]
       , HH.text ". It represents a fair cross-section of tunes that are regularly played at the session."
       ]
    ]
