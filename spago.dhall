{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "apollo"
    , "console"
    , "debug"
    , "effect"
    , "fixed-precision"
    , "graphql"
    , "psci-support"
    , "react-basic"
    , "react-basic-hooks"
    , "react-basic-native"
    , "tuples-native"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
