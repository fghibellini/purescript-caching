{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "caching"
, dependencies = [ "aff-retry", "console", "effect", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
