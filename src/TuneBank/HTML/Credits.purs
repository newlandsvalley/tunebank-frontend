module TuneBank.HTML.Credits (credits) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


credits :: forall i p. HH.HTML i p
credits =
  HH.div
    [ HP.id "creditsdiv" ]
    [ HH.p_
      [ HH.text "TradTuneDb uses "
      , HH.a
        [ HP.href "http://abcnotation.com/" ]
        [ HH.text "ABC notation" ]
      , HH.text " and so obviously owes a tremendous debt to Chris Walshaw who developed it. It uses the "
      , HH.a
        [ HP.href "https://github.com/newlandsvalley/musicrest" ]
        [ HH.text "MusicRest" ]
      , HH.text " web service for storing and transcoding its tunes. This in turn transcodes by means of:"
      ]
    , HH.div
      []
      [ HH.dl
        [HP.id "credits-examples"  ]
        [ HH.dt_
          [ HH.text "ABC to "
            , HH.a
              [ HP.href "http://en.wikipedia.org/wiki/PostScript" ]
              [ HH.text "postscript" ]
          ]
        , HH.dd_
           [ HH.a
              [ HP.href "http://moinejf.free.fr/" ]
              [ HH.text "abcm2ps" ]
           ]
        , HH.dt_
           [ HH.text "postscript to various" ]
        , HH.dd_
          [ HH.a
              [ HP.href "http://linux.about.com/od/commands/l/blcmdl1_convert.htm" ]
              [ HH.text "convert" ]
          ]
        , HH.dt_
          [ HH.text "ABC to "
            , HH.a
              [ HP.href "http://en.wikipedia.org/wiki/MIDI" ]
              [ HH.text "MIDI" ]
            ]
        , HH.td_
          [ HH.a
            [ HP.href "http://abc.sourceforge.net/abcMIDI/" ]
            [ HH.text "abcMIDI" ]
          ]
        ]
      ]
    , HH.p_
      [ HH.text "Code is written in PureScript and PureScript-Halogen. "
      , HH.text "The idea for the polska metronome was borrowed from Ben Potton of the band"
      , HH.a
        [ HP.href "http://www.jigfoot.com/" ]
        [ HH.text "JigFoot." ]
      , HH.text " The tune players create the melody by virtue of "
      , HH.a
        [ HP.href "https://github.com/gleitz/midi-js-soundfonts" ]
        [ HH.text "pre-rendered soundfonts" ]
      , HH.text " from Benjamin Gleitzman."
      ]

    ]
