module TuneBank.HTML.About (about) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (($), (<>))

about :: forall i p. HH.HTML i p
about =
  HH.div
    [ HP.id_ "aboutdiv" ]
    [ HH.h2_
        [ HH.text "TuneBank version 0.1.0" ]
    , HH.p_
       [ HH.text $
         ("TuneBank is a database of Irish, Scottish, Scandinavian and klezmer tunes. You can search for tunes and then view the scores " <>
          "or hear them played.  Of course, having them played on a rather tinny midi piano is no substitute for hearing the real thing, " <>
          "but it may help you to learn the basics of a tune because you can vary the tempo. Currently, the player works with the latest " <>
          " releases of all the major browsers other than Internet Explorer. The best quality sound output is to be found with Chrome")
       ]
    , HH.p_
      [ HH.text $
         ("TradTuneDb has been developed by Bayswater Software. You may like to add further music genres, or perhaps you have other suggestions" <>
          "for improvement or new features.Please ")
      , HH.a
         [ HP.href "mailto:john.watson@@gmx.co.uk"]
         [ HH.text "contact us"]
      , HH.text " and your request will be considered."
      ]
    ]
