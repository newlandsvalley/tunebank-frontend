tunebank-frontend
=================

**Work In Progress**

I'm considering an overhaul of the [tradtunedb](http://www.tradtunedb.org.uk/) website in order to freshen it up a bit and to make it more maintainable. This is an investigation into rewriting the front end (currently: Scala / Play with embedded PureScript) entirely in PureScript / Halogen 5.  Then I'll take a look at the backend.

During development, HTTP requests will be issued through the CORS anywhere proxy in order for them to be honoured.

Possible Improvements
---------------------
  * Cache the piano soundfont to make it available between pages.
  * Replace the Midi.js player with my PureScript SoundFont player.
  * Better integration of the ABC Editor.  Perhaps this should only be available for logged-in users. An edit button on the tune page would take you to the editor if you were the original author (or if you are an administrator). Add a button to save the edited tune to the database (if valid).
  * Add a 'select genre' button to the navigation menu and remove genre selection  from individual forms.
  * Add a 'select comments' button on the tune page which is initially closed but which can be opened so as to display all comments on the tune page itself.
  * Probably remove the Midi to ABC converter - it is not widely used.
  * Drop the 'new tune' menu button.  Text upload will still be available and now happens via the editor.
  * Make the css more friendly for mobile devices.

Stretch Goal Improvements (requiring a server change)
-----------------------------------------------------
  * Protect **all** resources with CORS headers to allow access from JavaScript-based clients.
  * Add role (e.g. admin) to the user profile.
  * Add preferred soundfont to the user profile and load it when the user logs in.
  * Possibly simplify the pagination interface.
  * Add tune thumbnails of the initial part of the score in tune searches.
  * Extend the JSON returned from tune searches.
  