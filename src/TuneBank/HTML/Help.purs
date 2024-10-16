module TuneBank.HTML.Help (help) where

import Prelude ((<>), ($))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


help :: forall i p. HH.HTML i p
help =
  HH.div
    [ HP.id "helpdiv"]
    [ HH.h2_
       [ HH.text "Why should I register?" ]
    , HH.text $
        ("You don't have to register to search for tunes, to see their scores and comments or to play them.  " <>
         "But you do need to register if you wish to submit a new tune or add a comment.  When you register, " <>
         "you will need to supply a user name and an email address.  We will keep your email address secret and will" <>
         "not spam you.  Once you send your details, you will be sent an email with a link which you must click on to finish the registration" <>
         "process.  Then you will be able to log in. You register via a link from the login page.")
    , HH.h2_
       [ HH.text "Which genres are supported?" ]
    , HH.text $
        ("At the moment, TradTuneDb recognises English, Irish, Klezmer, Scottish and (especially) Scandinavian tunes. " <>
        " These are kept in separate collections and you must select your chosen genre before searching for tunes." <>
        " Each tune has a distinct name (consisting of the tune name and its type) which is used in its URL. " <>
        " Also, each tune must have a key signature and a rhythm " <>
        "(the tune type). The rhythms available are appropriate for the genre (so, for example, Irish tunes use jigs, " <>
        " reels etc. whilst Scandi tunes use polska, gånglåt etc.).  You can optionally use the keys or the rhythms " <>
        " when searching for tunes. The search dropdowns show the rhythms and keys that are supported for each genre." <>
        " If you want to, you can also use an advanced search where (for example) you can look for a particular sequence of notes.")
    , HH.h2_
       [ HH.text "How do I search for a tune that I can hum but don't know anything about?" ]
    , HH.text $
       ("Probably the best way is simply to click on search and then, for each page of results, select the  " <>
       " add thumbnails button. This displays the first two or three bars of each tune in the list and you can click " <>
       " on a thumbnail to hear how the tune starts.")
    , HH.h2_
       [ HH.text "How do learn ABC?" ]
    , HH.text "You can use the interactive "
    , HH.a
      [ HP.href "abctutorial"]
      [ HH.text "ABC tutorial"]
    , HH.text $
       (" This takes you through a set of lessons, starting simply with just the letters ABC and so on " <>
        "and ending with a fully-fledged score.  Each lesson sample can be edited and if you enter incorrect " <>
        " text at any stage an error message is displayed - otherwise you can play the tune snippet.  This " <>
        " should teach you the basics - after this you could look at a more thorough tutorial - for example: ")
    , HH.a
      [ HP.href "http://abcnotation.com/learn"]
      [ HH.text "this one"]
    , HH.h2_
      [ HH.text "How do I create an ABC file?" ]
    , HH.text "You can use the interactive "
    , HH.a
      [ HP.href "abceditor"]
      [ HH.text "ABC editor"]
    , HH.text $
       ("This allows you to build up your ABC text gradually.  If it is correct, a player will appear allowing " <>
        "you to hear what you've written and see the score ; if incorrect, you  will see an error message.  " <>
        "You can also use it to change octave or transpose to a different key.  Once you are happy, you can save " <>
        "the ABC to file and load it to tradtunedb. This option is not available on mobile phones.")
    , HH.h2_
        [ HH.text "What restrictions are there in acceptable ABC?" ]
    , HH.ul_
       [ HH.li_
         [ HH.text ("Names in TradTuneDB are in unicode - you should use a unicode-aware editor. " <>
                   "Particularly important for Scandi tunes where a good many names are non-ASCII.")]
       , HH.li_
         [ HH.text ("Tunes must be submitted one at a time. They should have a single voice " <>
                     "(at the moment, harmony lines are not properly supported).")]
       , HH.li_
         [ HH.text "Tune headers must contain a title (T:)." ]
       , HH.li_
         [ HH.text ("Tune headers must contain a rhythm (R:) appropriate for the genre. " <>
                    "These are to be found in the rhythm dropdowns in the search page.") ]
       , HH.li_
         [ HH.text "Tune headers must contain a key (K:) indicator - e.g. GMaj, GMajor, GMinor, Gm. etc."]
       , HH.li_
         [ HH.text "Chord symbols in the tune body (e.g. \"Gm\" ) are not supported."]
       ]
    , HH.h2_
      [ HH.text "How do I submit a tune?" ]
    , HH.text $
       ("Once you have logged in, the Upload menu item becomes available from where you can upload an ABC file. " <>
        "Whereas the ABC Editor is very liberal, upload conditions are a good deal stricter (see the restrictions above). " <>
        "These are in place in order to ensure that tune searches are more accurate. " <>
        "and that the MIDI-player sounds reasonable."
       )
     , HH.h2_
       [ HH.text "How do I correct a tune I have submitted?" ]
     , HH.text
       ("Once you have submitted a tune successfully, you will see options on the tune page which allow you either " <>
        "to delete or edit the tune. The edit tune option takes you to the ABC editor mentioned above with your tune " <>
        "text in place. Simply make your changes, save the tune to file and then resubmit it. " <>
        "If you alter the tune title, there will, however, be two versions of the tune on the system because the title is " <>
        "used as a key. You should then delete the older version of the tune."
        )

    ]
