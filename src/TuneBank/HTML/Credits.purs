module TuneBank.HTML.Credits (credits) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


credits :: forall i p. HH.HTML i p
credits =
  HH.div
    [ HP.id_ "creditsdiv" ]
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
    , HH.table
      []
      [ HH.tr_
        [ HH.td_
          [ HH.text "ABC to " ]
          , HH.a
            [ HP.href "http://en.wikipedia.org/wiki/PostScript" ]
            [ HH.text "postscript" ]
        , HH.td_
          [ HH.a
            [ HP.href "http://moinejf.free.fr/" ]
            [ HH.text "abcm2ps" ]
          ]
        ]
      , HH.tr_
        [ HH.td_
           [ HH.text "postscript to various formats:" ]
        , HH.td_
          [ HH.a
              [ HP.href "http://linux.about.com/od/commands/l/blcmdl1_convert.htm" ]
              [ HH.text "convert" ]
          ]
        ]
      , HH.tr_
        [ HH.td_
          [ HH.text "ABC to " ]
          , HH.a
            [ HP.href "http://en.wikipedia.org/wiki/MIDI" ]
            [ HH.text "MIDI" ]
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





{-}
    <div id="creditsdiv" role="main" class="main" >
          <p>TradTuneDb uses <a href="http://abcnotation.com/">ABC notation</a> and so obviously owes a tremendous debt to Chris Walshaw who developed it.
 It uses the <a href="https://github.com/newlandsvalley/musicrest">MusicRest</a> web service for storing and transcoding its tunes. This in turn transcodes by means of:</p>
          <table id="samplesearch" >
             <tr>
                <td>ABC to <a href="http://en.wikipedia.org/wiki/PostScript">postscript:</a></td>
                <td><a href="http://moinejf.free.fr/">abcm2ps</a></td>
             </tr>
             <tr>
                <td>postscript to various formats:</td>
                <td><a href="http://linux.about.com/od/commands/l/blcmdl1_convert.htm">convert</a></td>
             </tr>
             <tr>
                <td>ABC to <a href="http://en.wikipedia.org/wiki/MIDI">midi:</a></td>
                <td><a href="http://abc.sourceforge.net/abcMIDI/">abcMIDI</a></td>
             </tr>
          </table>
          <p>It uses the <a href="http://mudcu.be/midi-js/">MIDI.js</a> midi player to play tunes which have been saved to the database and the soundfont
              player from  <a href="https://github.com/newlandsvalley/purescript-halogen-components">purescript-halogen-components</a> for tunes played
              in the ABC editor and tutorial. It uses <a href="http://www.vexflow.com/">VexFlow</a> for dynamic score engraving in the editor.</p>
          <p>Code is written in scala and <a href="http://www.purescript.org/">PureScript</a>, with particular thanks to the following:</p>
          <table id="samplesearch" >
             <tr>
                <td>content negotiation:</td>
                <td><a href="http://spray.io/">spray</a></td>
             </tr>
             <tr>
                <td>web framework:</td>
                <td><a href="http://www.playframework.org/">play</a></td>
             </tr>
             <tr>
                <td>interface to the <a href="http://www.mongodb.org/" >mongoDB</a> database:</td>
                <td><a href="http://api.mongodb.org/scala/casbah/2.0/">casbah</a></td>
             </tr>
             <tr>
                <td>JSON:</td>
                <td><a href="http://argonaut.io/">argonaut</a></td>
             </tr>
          </table>
          <p>The idea for the polska metronome was borrowed from Ben Potton of the band
            <a href="http://www.jigfoot.com/">Jigfoot</a>. </p>

    </div>
-}
