module Caching.CachedMap
  ( Config
  , CachedMap
  , new
  , read
  , size
  ) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as A
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as AVar
import Effect.Aff (Aff, delay, forkAff, launchAff_, throwError, try)
import Effect.Aff.AVar as AVarAff
import Effect.Class (liftEffect)
import Effect.Exception (error, Error)
import Effect.Now as EffectNow
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)

-- TODO
-- ----
-- Periodically log information about the CachedMap
-- so we can observe for instance if they start growing too big,
-- or too many values are being cleaned up by the reaper process.
type Config =
  { reaperInterval ∷ Milliseconds -- ^ How often are expired records deallocated
  , ttl ∷ Milliseconds -- ^ For how long are values considered valid after they are fetched
  }

-- | A new AVar is created for each fetch.
-- | The AVar is empty at first so that other attempts to read,
-- | will simply block util the fetch completes.
-- | Any time a new attempt at fetching is performed the Slot in the map is replaced
-- | and the AVar of the former Slot is killed (atomically).
type Slot a =
  { value ∷ AVar a
  , fetchTime ∷ Instant -- when was the value fetched
  }

-- | A @CachedMap@ is a Map-like structure whose each value
-- | can be fetched through an Aff computation accepting the key.
-- | The structure starts-off empty and values are fetched whenever
-- | they are requested and a non-expired value is not available.
-- |
-- | Multiple reads to the same non-cached value trigger only one fetch call.
-- |
-- | Fetch failure will simply fail the @read@ call i.e. recovery mechanism must be implemented on client's side.
-- |
-- | A background reaper process is periodically triggered and it will iterate
-- | over all the records and clean the ones that are expired. This prevents the
-- | cache from growing indefinitely.
data CachedMap k a
  = CachedMap
    { ref ∷ Ref (Map k (Slot a))
    , ttl ∷ Milliseconds
    , fetchFn ∷ k -> Aff a
    }

new ∷ ∀ k a. Ord k => Config -> (k -> Aff a) -> Effect (CachedMap k a)
new { ttl, reaperInterval } fetchFn = do
  ref <- Ref.new Map.empty
  let
    sv = CachedMap { ref, ttl, fetchFn }
  launchAff_ $ reaperProcess ref
  pure sv
  where
    reaperProcess ref =
      forever do
        delay reaperInterval
        liftEffect do
          now <- EffectNow.now
          cache <- Ref.read ref
          cache' <- removeStale now cache
          Ref.write cache' ref

    removeStale ∷ Instant -> Map k (Slot a) -> Effect (Map k (Slot a))
    removeStale now =
      toUnfoldable
        >>> traverse
            ( \slot@(Tuple k v) ->
                if stillValid ttl now v then
                  pure (Just slot)
                else do
                  AVar.kill expiredError v.value
                  pure Nothing
            )
        >>> map A.catMaybes
        >>> map fromFoldable

    expiredError ∷ Error
    expiredError = error "You attempted to read an expired AVar. This should never happen."

stillValid ∷ ∀ a. Milliseconds -> Instant -> Slot a -> Boolean
stillValid ttl now slot = (instantAddMs slot.fetchTime ttl) > now

read ∷ ∀ k a. Ord k => k -> CachedMap k a -> Aff a
read key (CachedMap { ref, ttl, fetchFn }) = do
  cache <- liftEffect $ Ref.read ref
  now <- liftEffect EffectNow.now
  case Map.lookup key cache of
    Just slot@{ value, fetchTime }
      | stillValid ttl now slot -> AVarAff.read value
    lookupResult -> do
      -- if the key is missing or expired always create a new AVar
      avar <- liftEffect AVar.empty
      -- we already update the ref such that any reads from now on
      -- will "subscribe" to the resolution of our fetch
      -- If there was already a slot that expired we need to notify the subscribers of that AVar by calling `kill` on it.
      liftEffect $ void $ for lookupResult \{ value } -> AVar.kill (error "Trying to access expired value") value
      _ <- liftEffect $ Ref.write (Map.insert key { value: avar, fetchTime: now } cache) ref
      _ <-
        forkAff do
          -- fetch and resolve the AVar
          fetchRes <- try (fetchFn key)
          avarIsKilled <- AVar.isKilled <$> liftEffect (AVar.status avar)
          unless avarIsKilled
            $ case fetchRes of
                Right res -> do
                  putRes <- liftEffect $ AVar.tryPut res avar
                  if putRes then
                    pure unit
                  else
                    throwError $ error "Tried to write to already populated AVar!" -- this should never happen
                Left err ->
                  liftEffect do
                    AVar.kill err avar -- notify any subscribers of the failure
                    _ <- Ref.write (Map.delete key cache) ref
                    pure unit
      AVarAff.read avar

size ∷ ∀ k a. CachedMap k a -> Effect Int
size (CachedMap { ref }) = Map.size <$> Ref.read ref

instantAddMs ∷ Instant -> Milliseconds -> Instant
instantAddMs x ms = unsafePartial $ fromJust $ instant (unInstant x <> ms)
