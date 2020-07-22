module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Aff (launchAff_, delay)


import Test.CachedMapSpec as CachedMapSpec

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    CachedMapSpec.spec
