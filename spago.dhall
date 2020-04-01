{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "apollo"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "fixed-precision"
  , "foreign-object"
  , "formatters"
  , "graphql"
  , "psci-support"
  , "react-basic"
  , "react-basic-hooks"
  , "react-basic-native"
  , "tuples-native"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
