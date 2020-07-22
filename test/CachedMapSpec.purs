module Test.CachedMapSpec where

import Prelude
import Control.Parallel (parSequence)
import Data.Either (isLeft, isRight)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff (delay, error, never, throwError, try)
import Effect.Class (liftEffect)
import Effect.Ref as IORef
import Caching.CachedMap as CachedMap
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

spec ∷ Spec Unit
spec =
  describe "CachedMap" do
    describe "Basic behavior" do
      it "works in happy case" do
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 1000.0, reaperInterval: Milliseconds 10000.0 } (\_ -> pure 42)
        v <- CachedMap.read "key1" cache
        v `shouldEqual` 42
      it "querying twice the same key should perform a single fetch" do
        counter <- liftEffect $ IORef.new 0
        let
          fetchFn key = liftEffect (42 <$ IORef.modify (_ + 1) counter)
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 1000.0, reaperInterval: Milliseconds 10000.0 } fetchFn
        vs <-
          parSequence
            [ CachedMap.read "key1" cache
            , CachedMap.read "key1" cache
            ]
        vs `shouldEqual` [ 42, 42 ]
        liftEffect (IORef.read counter) >>= (_ `shouldEqual` 1)
      it "querying two different values should perform two fetches" do
        counter <- liftEffect $ IORef.new 0
        let
          fetchFn key = liftEffect (42 <$ IORef.modify (_ + 1) counter)
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 1000.0, reaperInterval: Milliseconds 10000.0 } fetchFn
        vs <-
          parSequence
            [ CachedMap.read "key1" cache
            , CachedMap.read "key2" cache
            ]
        vs `shouldEqual` [ 42, 42 ]
        liftEffect (IORef.read counter) >>= (_ `shouldEqual` 2)
      it "querying the same key after expiration should trigger a second fetch" do
        counter <- liftEffect $ IORef.new 0
        let
          fetchFn key = liftEffect (42 <$ IORef.modify (_ + 1) counter)
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 50.0, reaperInterval: Milliseconds 10000.0 } fetchFn
        v1 <- CachedMap.read "key1" cache
        delay (Milliseconds 70.0)
        v2 <- CachedMap.read "key1" cache
        liftEffect (IORef.read counter) >>= (_ `shouldEqual` 2)
      it "should report a size of 0 on a newly created map" do
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 50.0, reaperInterval: Milliseconds 10000.0 } (\(_ ∷ String) -> pure 42)
        liftEffect (CachedMap.size cache) >>= (_ `shouldEqual` 0)
    describe "Reaper" do
      it "should clean up expired records" do
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 50.0, reaperInterval: Milliseconds 100.0 } (\_ -> pure 42)
        -- new record
        v <- CachedMap.read "key1" cache
        liftEffect (CachedMap.size cache) >>= (_ `shouldEqual` 1)
        -- record is expired but not cleaned up until reaper kicks in
        delay (Milliseconds 70.0)
        liftEffect (CachedMap.size cache) >>= (_ `shouldEqual` 1)
        -- reaper cleans up record
        delay (Milliseconds 40.0)
        liftEffect (CachedMap.size cache) >>= (_ `shouldEqual` 0)
      it "should not clean up records that were updated" do
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 50.0, reaperInterval: Milliseconds 100.0 } (\_ -> pure 42)
        -- new record
        v <- CachedMap.read "key1" cache
        liftEffect (CachedMap.size cache) >>= (_ `shouldEqual` 1)
        -- record is expired but not cleaned up until reaper kicks in
        delay (Milliseconds 70.0)
        liftEffect (CachedMap.size cache) >>= (_ `shouldEqual` 1)
        -- force refetch
        v' <- CachedMap.read "key1" cache
        -- reaper does not clean up record
        delay (Milliseconds 40.0)
        liftEffect (CachedMap.size cache) >>= (_ `shouldEqual` 1)
    describe "Errors" do
      it "should throw an exception when the fetch fails" do
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 1000.0, reaperInterval: Milliseconds 10000.0 } (\_ -> throwError $ error "Service not available 1")
        res <- try (CachedMap.read "key1" cache)
        ("msg" <$ res) `shouldSatisfy` isLeft
      it "should fail with an exception even if it didn't trigger the fetch" do
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 1000.0, reaperInterval: Milliseconds 10000.0 } (\_ -> throwError $ error "Service not available 2")
        rs <-
          parSequence
            [ try (CachedMap.read "key1" cache)
            , try (CachedMap.read "key1" cache)
            ]
        for_ rs \res -> do
          ("msg" <$ res) `shouldSatisfy` isLeft
      it "should throw exceptions on reads if the reaper cleans up an ongoing but expired attempt" do
        let
          fetchFn key = never
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 50.0, reaperInterval: Milliseconds 100.0 } fetchFn
        res <- try (CachedMap.read "key1" cache)
        ("msg" <$ res) `shouldSatisfy` isLeft -- read failed even though the fetch never terminated
      it "should throw an exception on reads if a following read cleans up an ongoing but expired attempt" do
        -- first fetch will never terminate
        -- all subsequent will resolve to 42
        counter <- liftEffect $ IORef.new 0
        let
          fetchFn key =
            liftEffect (IORef.modify (_ + 1) counter)
              >>= case _ of
                  1 -> never
                  _ -> pure 42
        cache <- liftEffect $ CachedMap.new { ttl: Milliseconds 50.0, reaperInterval: Milliseconds 100.0 } fetchFn
        rs <-
          parSequence
            [ try (CachedMap.read "key1" cache)
            , delay (Milliseconds 70.0) *> try (CachedMap.read "key1" cache)
            ]
        case rs of
          [ r1, r2 ] -> do
            r1 `shouldSatisfy` isLeft -- first one failed even though it never terminated
            r2 `shouldSatisfy` isRight
          _ -> do
            throwError $ error "Impossible has happened: parSequence should not change the number of elements"
