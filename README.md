tunebank-frontend
=================

**Work In Progress**

I'm considering an overhaul of the [tradtunedb](http://www.tradtunedb.org.uk/) website in order to freshen it up a bit and to make it more maintainable. This is an investigation into rewriting the front end (currently: Scala / Play with embedded PureScript) entirely in PureScript / Halogen 5.  Then I'll take a look at the backend.

Possible Improvements
---------------------
  * Cache the piano soundfont to make it available between pages.
  * Replace the Midi.js player with my SoundFont player.
  * Better integration of the ABC Editor.  Perhaps this should only be available for logged-in users. An edit button on the tune page would take you to the editor if you were the original author (or if you are an administrator). Add a button to save the edited tune to the database (if valid).
  * Add a 'select genre' button to the navigation menu and remove genre selection  from individual forms.
  * Add a 'select comments' button on the tune page which is initially closed but which can be opened so as to display all comments on the tune page itself.
  * Possibly remove the Midi to ABC converter - it is not widely used.
  * Drop the 'new tune' menu button.  Upload will still be available and now also happens via the editor.

Stretch Goal Improvements (requiring a server change)
-----------------------------------------------------
  * Add role (e.g. admin) to the user profile,
  * Add preferred soundfont to the user profile and load it when the user logs in.
  * Possibly simplify the pagination interface.
  