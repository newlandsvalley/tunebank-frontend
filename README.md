tunebank-frontend
=================

[try it here](http://www.tradtunedb.org.uk:8604)

This is the browser frontend code for the [tradtunedb](http://www.tradtunedb.org.uk/) website written entirely in PureScript / Halogen.

This has been made possible by adding CORS header protection to the MusicRest server to all resources that a JavaScript frontend needs to access.

The latest release attempts to provide a responsive design, making it accessible to mobile devices.

To Build
--------

    npm run build-legacy

To Test
-------

    fire up a tradtunedb development server then:

    npm run test-legacy

Still to do (possibly requiring further server changes)
-------------------------------------------------------
  * Add role (e.g. admin) to the user profile.
  * Add preferred soundfont to the user profile and load it when the user logs in.
