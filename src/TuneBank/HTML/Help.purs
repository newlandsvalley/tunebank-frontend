module TuneBank.HTML.Help (help) where

import Prelude ((<>), ($))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


help :: forall i p. HH.HTML i p
help =
  HH.div
    [ HP.id_ "helpdiv"]
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
        ("At the moment, TradTuneDb recognises Irish, Klezmer, Scottish and (especially) Scandinavian tunes. " <>
        " These are kept in separate collections and you must select your chosen genre before searching for tunes." <>
        " Each tune has a distinct name (consisting of the tune name and its type) which is used in its URL. " <>
        " Also, each tune must have a key signature and a rhythm " <>
        "(the tune type). The rhythms available are appropriate for the genre (so, for example, Irish tunes use jigs, " <>
        " reels etc. whilst Scandi tunes use polska, gånglåt etc.).  You can optionally use the keys or the rhythms " <>
        " when searching for tunes. The search dropdowns show the rhythms and keys that are supported for each genre." <>
        " If you want to, you can also use an advanced search where (for example) you can look for a particular sequence of notes.")
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
        "the ABC to file and load it to tradtunedb.")
    , HH.h2_
      [ HH.text "How do I submit a tune?" ]
    , HH.text $
       ("Once you have logged in, the Upload menu item becomes available from where you can upload an ABC file. " <>
        "Whereas the ABC Editor is very liberal, upload conditions are a good deal stricter.  You must ensure that the " <>
        "ABC headers include X (reference number), T (tune title), K (key) and R (rhythm) and that the rhythm is appropriate " <>
        "to the genre in question. This is to ensure that tune searches are more accurate. The file should include only a single tune. " <>
        "Note, too, that TradTuneDB is unicode-aware and so you should be careful to upload tunes that have been prepared by a " <>
        "unicode-aware editor. This is particularly important for Scandi tunes where a good many names are non-ASCII."
       )
    ]

{-}
<div id="helpdiv" role="main" class="main" >
       <h2>Are there any known issues or restrictions?</h2>
       <p>Tunes cannot be played in Internet Explorer. Because this is probably the main reason for using the system in the first
       place, you are strongly recommended to use a different browser.
       </p>
       <p>The <em>try it out</em> button for both new tunes and comments doesn't work in Chrome Version 30.  Instead, the tune or the comment is added to
       the database.  Other browsers and previous versions of Chrome are OK.  It is hoped that this will be fixed in the next version of Chrome.
       </p>
       <h2>Why should I register?</h2>
       <p>You don't have to register to search for tunes, to see their scores and comments or to play them.  But you do need to register if you wish to submit a
       new tune or add a comment.  When you register, you will need to supply a user name and an email address.  We will keep your email address secret and will
       not spam you.  Once you send your details, you will be sent an email with a link which you must click on to finish the registration
       process.  Then you will be able to log in. You register via a link from the login page.
       </p>
       <h2>Which genres are supported?</h2>
       <p>At the moment, TradTuneDB recognises Irish, Scottish, Scandinavian and klezmer tunes.  These are kept in separate collections and when you choose
       a particular genre in a tune search, this is used as the default until you decide to change it.  Each tune has a distinct name (consisting of the
       tune name and its type) which is used in its URL.  Also, each tune must have a key signature and a rhythm (the tune type).
       The rhythms available are appropriate for the genre (so, for example, Irish tunes use jigs, reels etc. whilst Scandi tunes use polska, gånglåt etc.).  You
       can optionally use the keys or the rhythms when searching for tunes. The search dropdowns show the rhythms and keys that are supported for each genre.
       If you want to, you can also use an advanced search where (for example) you can look
       for a particular sequence of notes.
       </p>
       <h2>How do learn ABC?</h2>
       <p>You can use the interactive <a href='abctutorial'>ABC tutorial</a>. This takes you through a set of lessons, starting simply with just the letters
       <em>ABC</em> and so on and ending with a fully-fledged score.  Each lesson sample can be edited and if you enter incorrect text at any stage an
       error message is displayed - otherwise you can play the tune snippet.  This should teach you the basics - after this you could look at a more
       thorough tutorial - for example <a href='http://abcnotation.com/learn'>this one</a>.
       </p>
       <h2>How do I create an ABC file?</h2>
       <p>You can experiment by using the interactive <a href='abceditor'>ABC editor</a>.  This allows you to build up your
       ABC text gradually.  If it is correct, a player will appear allowing you to hear what you've written; if incorrect, you
       will see an error message.  You can also use it to change octave or transpose to a different key.  Once you are happy,
       you can save the ABC to file and load it to tradtunedb in the normal fashion.
       </p>
       <p>Note that the editor is very liberal in the ABC that it accepts.  For example, it allows you to enter and play just the notes whereas a complete
       ABC tune requires certain headers always to be present.  It also plays tunes very simply - so that (for example) chord symbols and grace notes are
       ignored.
       </p>
       <h2>How do I submit a tune?</h2>
       <p>This is slightly more complicated.  You must, of course, first log in. Tunes are submitted in <a href='http://abcnotation.com/'>ABC notation</a> which
       is a pure text format consisting of a set of headers describing the tune followed by the notes which are for the most part represented by the letters A to G.
       So the first problem is to get hold of an ABC file that defines your tune.  Nowadays there are a good many traditional music web sites that allow you to
       download an ABC file.  You could, perhaps, do this so that you can then play the tune in TradTuneDb.
       </p>
       <p>If you choose to do this, there are one or two things to be wary of.  Firstly, make sure the ABC file only contains a single tune (TradTuneDB doesn't
       allow you to upload multiple tunes from the same file).  Make sure that the basic set of tune headers are present.  These are:

         <table class="textalign">
           <tr><td>T</td><td>Title</td><td>e.g. T:Polska fran Hälleforsnäs</td></tr>
           <tr><td>K</td><td>Key</td><td>e.g. K:Amin</td></tr>
           <tr><td>R</td><td>Rhythm</td><td>e.g. R: Polska</td></tr>
         </table>
         <br />

       Notice that here the tune has a non-ASCII title - you will need to use a Unicode-aware text editor to enter such names properly.  Make sure that the
       key and the genre is supported by TradTuneDB. ABC notation offers quite a lot of sophistication
       in representing the tune's score (such as chords, harmonies, ornamentation and so on).  However, it is usually
       best if you don't use these features if you want to hear the tune played clearly - just use tunes with a single melody line.
       </p>
       <p>By default, the player will play the tune as closely as possible to a tempo of 1/4=120 (i.e. 120 crochets to the minute). If you'd prefer to
       change this, then you can supply a <b>Q</b> header in the ABC:

         <table class="textalign">
           <tr><td>Q</td><td>Tempo</td><td>e.g. Q:1/4=120</td></tr>
         </table>
         <br />

       and of course you can also change the speed by moving the slider.
       </p>
       <p>If you can't find a ready-made ABC file but you do have a midi recording of the tune, you can try to <a href='helpmidi2abc'>convert the midi</a>
       directly into ABC. Otherwise, you will have to build the ABC yourself from scratch.  This usually means you first have to transcribe the tune and
       then it is a largely mechanical process to convert the score.   Your final file should have the extension <em>.txt</em> although some browsers will also accept the <em>.abc</em> extension.
       </p>
       <p>Now you've got your ABC file, you can try to upload it to TradTuneDB.  Firstly, make sure you are logged in and that you are using your intended genre.
       Then, you can either use the <em>new tune</em> form and paste the ABC into the form field, or you can use the <em>upload</em> option where you enter the location
       of the ABC file on your file system. If the ABC checks out OK, the system immediately shows you the tune's page, allowing you to see the score and hear it.
       You may find that the tune doesn't look or sound quite right and you'd like to tweak the notes a little. To do this, simply edit the ABC and resubmit it.
       You can do this as many times as you like until you are happy with your transcription.  You may also delete the tune if you're unhappy with it by navigating
       to the ABC page for the tune and selecting <em>delete</em>.
       Alternatively, the new tune page has a button labelled 'try it out' which allows you to look at the score but doesn't save the tune
       to the database.  Again you can keep trying with edited versions of the tune until you are happy with the score and can submit it.
       </p>

       <h2>How do I submit a comment?</h2>
       <p>Comments are intended to hold additional information about each tune.  They are particularly useful if you find a YouTube video of the tune
       or perhaps a link to a recording.  Again, you have to log in before you can submit a comment.  Once on the tune page, click on the comments link
       and fill in the form at the foot of the page.  In the case of YouTube URLs, the best thing to do is to embed the video by
       <a href='https://support.google.com/youtube/answer/171780?hl=en-GB'>following these instructions</a> and then copy the code
       they provide into the text. Comments should normally be in plain text but may include URLs - these are expanded to links when the comment
       is submitted and this is true, too, for YouTube <em>watch</em> URLs that you see in the browser address bar. We will attempt to embed the player into the
       page at a standard size, but this method is less reliable than using YouTube's own embedded links.
       </p>
       <p> If you are not happy with a comment that you have submitted, you can later edit or delete it.  You can also 'try it out' to see what the comment
       will eventually look like before finally  committing it to the comment list.
       </p>

-}
