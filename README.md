tunebank-frontend
=================

[try it here](https://www.tradtunedb.org.uk)

This is the browser frontend code for the [tradtunedb](https://www.tradtunedb.org.uk/) website written entirely in PureScript / Halogen.

This has been made possible by adding CORS header protection to the MusicRest server to all resources that a JavaScript frontend needs to access.  Although now this is unnecessary because there is now a reverse proxy placed between it and MusicRest.

The latest release attempts to provide a responsive design, making it accessible to mobile devices.

To Build
--------

    npm run build

To Test
-------

    fire up a tradtunedb development server then:

    npm run test

Still to do (possibly requiring further server changes)
-------------------------------------------------------
  * Add role (e.g. admin) to the user profile.
  * Add preferred soundfont to the user profile and load it when the user logs in.
