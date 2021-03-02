{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "integers"
  , "lists"
  , "node-fs"
  , "numbers"
  , "psci-support"
  , "random"
  , "strings"
  , "strings-extra"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
