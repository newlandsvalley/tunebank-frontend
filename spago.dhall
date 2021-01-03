{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "tunebank-frontend"
, dependencies =
  [ "abc-editor"
  , "abc-melody"
  , "abc-parser"
  , "abc-scores"
  , "abc-tutorial"
  , "affjax"
  , "argonaut"
  , "debug"
  , "effect"
  , "expand-links"
  , "formatters"
  , "halogen"
  , "halogen-css"
  , "halogen-components"
  , "html-parser-halogen"
  , "numbers"
  , "polska-metronome"
  , "prelude"
  , "routing"
  , "routing-duplex"
  , "soundfonts"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
