tunebank-frontend
=================

This branch is for the variant of tunebank-frontend that attaches to the Haskell tunebank server. The idea is eventually to migrate the Scala backend so that the two backends provide exactly the same API as far as tunebank-frontend is concerned.  Progress on this is stymied at the moment because we still need to retain support for the Scala client - __tradtunedb__.

Differences between the Scala and Haskell compliant branches of tunebank-frontend:

  * The ABCMetadata JSON differs.  In the Scala version, ABC headers are identified by the ABC header code (e.g. `T`) whilst in the Haskell version they are identified by name (e.g. `title`).
  * Tune search parameters differ.  Again, in the Scala version they are identified by header code, which, because they are capitalised, breaks HTTP norms.  In the Haskell version, they are identified by name.
  * The `checkUser` login check request differs.  The Haskell version correctly returns 403 (Forbidden) for bad credentials but the Scala version does not (just returning Y/N).  Consequently `checkUser` is improved.
  * The `valid` field in a JSON `UserRef` (in lists of users) is a Boolean in the Haskell backend (it is a String in the Scala backend).


[try it here](http://www.tradtunedb.org.uk:8604)

This is an overhaul of the [tradtunedb](http://www.tradtunedb.org.uk/) website which freshens it up a bit and makes it more maintainable. The front end (currently: Scala / Play with embedded PureScript) is now written entirely in PureScript / Halogen 5.

This has been made possible by adding CORS header protection to the MusicRest server to all resources that a JavaScript frontend needs to access.

Still to do (possibly requiring further server changes)
-------------------------------------------------------
  * Make the css more friendly for mobile devices.
  * Add role (e.g. admin) to the user profile.
  * Add preferred soundfont to the user profile and load it when the user logs in.
  * Possibly simplify the pagination interface.
