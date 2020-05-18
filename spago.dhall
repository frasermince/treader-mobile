{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "affjax"
    , "apollo"
    , "console"
    , "datetime"
    , "debug"
    , "effect"
    , "fixed-precision"
    , "foreign-object"
    , "formatters"
    , "globals"
    , "graphql"
    , "interpolate"
    , "ordered-collections"
    , "psci-support"
    , "random"
    , "react-basic"
    , "react-basic-hooks"
    , "react-basic-native"
    , "record"
    , "stringutils"
    , "tuples-native"
    , "typelevel"
    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
