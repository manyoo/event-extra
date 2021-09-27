{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "event"
  , "filterable"
  , "foldable-traversable"
  , "integers"
  , "js-timers"
  , "maybe"
  , "now"
  , "partial"
  , "prelude"
  , "psci-support"
  , "refs"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
