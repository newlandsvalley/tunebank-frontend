{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230808/packages.dhall
        sha256:1f2f8be27ed474d333848bfae9db43790202e3b4da8d73584b1029260677d7e1

in  upstream
  with abc-melody =
    { dependencies =
      [ "abc-parser"
      , "arrays"
      , "bifunctors"
      , "either"
      , "foldable-traversable"
      , "integers"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "rationals"
      , "rhythm-guitar"
      , "soundfonts"
      , "transformers"
      , "tuples"
      ]
    , repo = "https://github.com/newlandsvalley/purescript-abc-melody.git"
    , version = "v0.3.1"
    }
  with rhythm-guitar =
    { dependencies =
      [ "aff"
      , "affjax"
      , "affjax-web"
      , "arrays"
      , "console"
      , "control"
      , "effect"
      , "either"
      , "foreign"
      , "http-methods"
      , "maybe"
      , "ordered-collections"
      , "prelude"
      , "soundfonts"
      , "simple-json"
      , "string-parsers"
      , "strings"
      , "tuples"
      ]
    , repo = "https://github.com/newlandsvalley/RhythmGuitar.git"
    , version = "v0.2.1"
    }
  with abc-scores =
    { dependencies =
      [ "abc-parser"
      , "arrays"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "integers"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "profunctor-lenses"
      , "rationals"
      , "safe-coerce"
      , "strings"
      , "stringutils"
      , "transformers"
      , "tuples"
      , "unfoldable"
      ]
    , repo = "https://github.com/newlandsvalley/purescript-abc-scores.git"
    , version = "v0.6.6"
    }
  with abc-editor =
    { dependencies =
      [ "abc-melody"
      , "abc-parser"
      , "abc-scores"
      , "aff"
      , "arrays"
      , "colors"
      , "console"
      , "css"
      , "dom-indexed"
      , "effect"
      , "either"
      , "halogen"
      , "halogen-components"
      , "halogen-css"
      , "integers"
      , "js-fileio"
      , "lists"
      , "maybe"
      , "media-types"
      , "partial"
      , "prelude"
      , "soundfonts"
      , "string-parsers"
      , "strings"
      ]
    , repo = "https://github.com/newlandsvalley/purescript-abc-editor.git"
    , version = "v1.1.2"
    }
  with abc-tutorial =
    { dependencies =
      [ "abc-melody"
      , "abc-parser"
      , "aff"
      , "arrays"
      , "colors"
      , "const"
      , "css"
      , "either"
      , "halogen"
      , "halogen-components"
      , "halogen-css"
      , "lists"
      , "maybe"
      , "ordered-collections"
      , "prelude"
      , "soundfonts"
      , "string-parsers"
      , "strings"
      ]
    , repo = "https://github.com/newlandsvalley/purescript-abc-tutorial.git"
    , version = "v1.1.2"
    }
  with expand-links =
    { dependencies = [ "console", "effect", "prelude", "strings" ]
    , repo = "https://github.com/newlandsvalley/purescript-expand-links.git"
    , version = "v0.2.0"
    }
  with halogen-components =
    { dependencies =
      [ "aff"
      , "arrays"
      , "css"
      , "datetime"
      , "dom-indexed"
      , "effect"
      , "halogen"
      , "halogen-css"
      , "integers"
      , "js-fileio"
      , "lists"
      , "maybe"
      , "nonempty"
      , "prelude"
      , "soundfonts"
      , "transformers"
      , "web-dom"
      , "web-html"
      , "web-uievents"
      ]
    , repo =
        "https://github.com/newlandsvalley/purescript-halogen-components.git"
    , version = "v0.6.0"
    }
  with polska-metronome =
    { dependencies =
      [ "aff"
      , "affjax"
      , "affjax-web"
      , "arrays"
      , "canvas"
      , "colors"
      , "datetime"
      , "drawing"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "halogen"
      , "http-methods"
      , "hyrule"
      , "integers"
      , "js-timers"
      , "maybe"
      , "newtype"
      , "numbers"
      , "ordered-collections"
      , "parallel"
      , "partial"
      , "prelude"
      , "transformers"
      , "tuples"
      , "webaudio"
      ]
    , repo = "https://github.com/newlandsvalley/purescript-polska-metronome.git"
    , version = "v0.3.4"
    }
  with webaudio =
    { dependencies =
      [ "aff"
      , "arraybuffer"
      , "arraybuffer-types"
      , "effect"
      , "maybe"
      , "prelude"
      , "psci-support"
      , "web-events"
      ]
    , repo = "https://github.com/newlandsvalley/purescript-webaudio.git"
    , version = "b2cf7c19314613834762680acadf397173978efd"
    }
  with drawing =
    { dependencies = [ "canvas", "lists", "numbers", "integers", "colors" ]
    , repo = "https://github.com/newlandsvalley/purescript-drawing"
    , version = "v6.0.0"
    }
  with html-parser-halogen =
    { dependencies =
      [ "arrays"
      , "control"
      , "dom-indexed"
      , "foldable-traversable"
      , "effect"
      , "halogen"
      , "maybe"
      , "prelude"
      , "psci-support"
      , "jest"
      ]
    , repo = "https://github.com/rnons/purescript-html-parser-halogen"
    , version = "035a51d02ba9f8b70c3ffd9fe31a3f5bed19941c"
    }
