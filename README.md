tunebank-frontend
=================

[try it here](http://www.tradtunedb.org.uk:8604)

This is an overhaul of the [tradtunedb](http://www.tradtunedb.org.uk/) website which freshens it up a bit and makes it more maintainable. The front end (previously: Scala / Play with embedded PureScript) is now written entirely in PureScript / Halogen 5.

This has been made possible by adding CORS header protection to the MusicRest server to all resources that a JavaScript frontend needs to access.

Still to do (possibly requiring further server changes)
-------------------------------------------------------
  * Make the css more friendly for mobile devices.
  * Add role (e.g. admin) to the user profile.
  * Add preferred soundfont to the user profile and load it when the user logs in.
  * Possibly simplify the pagination interface.
