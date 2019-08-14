tunebank-frontend
=================

**Work In Progress**

This is an overhaul of the [tradtunedb](http://www.tradtunedb.org.uk/) website which freshens it up a bit and makes it more maintainable. The front end (currently: Scala / Play with embedded PureScript) is now written entirely in PureScript / Halogen 5. 

This has been made possible by adding CORS header protection to the MusicRest server to all resources that a JavaScript frontend needs to access. 

Improvements Completed
----------------------
  * Cache the piano soundfont to make it available between pages.
  * Replace the Midi.js player with my PureScript SoundFont player.
  * Add a 'select genre' button to the navigation menu and remove genre selection from individual forms.
  * Display all comments on the tune page itself rather than on a linked comments page.
  * Remove the Midi to ABC converter - it is not widely used.
  * Drop the 'new tune' menu button.  Text upload will still be available and now happens via the editor.
  * Extend the JSON returned from the server in tune searches to enable thumbnails.
  * Add tune thumbnails of the initial few bars of the score in tune searches.
  * Improve the ABC parsing such that very few tunes currently in the database fail to parse.

To do (possibly requiring further server changes)
-----------------------------------------------------
  * Better integration of the ABC Editor.  Perhaps this should only be available for logged-in users. An edit button on the tune page would take you to the editor if you were the original author (or if you are an administrator). 
  * Integrate the polska metronome and ABC tutorial.
  * Make the css more friendly for mobile devices.
  * Add role (e.g. admin) to the user profile.
  * Add preferred soundfont to the user profile and load it when the user logs in.
  * Possibly simplify the pagination interface.
  