module Caching.SynchedValue
  ( SynchedValue
  , new
  , awaitNew
  , read
  , isEmpty
  ) where

-- | TODO
-- | Stats about @SynchedValue@s should be exposed on the EKG page.

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.DateTime (DateTime)
import Data.Maybe (isNothing)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Effect (Effect)
import Effect.AVar (AVar, tryRead)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Aff.Retry (capDelay, fullJitterBackoff, recovering)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDateTime)

data Slot a
  = Slot
    { value ∷ a
    , requestedAt ∷ DateTime
    , resolvedAt ∷ DateTime
    }

-- | A @SynchedValue@ is a mutable variable that is periodically
-- | updated by a background process that calls the user-supplied
-- | function.
-- | @refresh_interval@ specifies the delay between consecutive updates.
data SynchedValue a
  = SynchedValue { avar ∷ AVar (Slot a) }

new ∷ ∀ a. Milliseconds -> Aff a -> Effect (SynchedValue a)
new refresh_interval update_function = do
  avar <- EffectAVar.empty
  launchAff_ $ forever
    $ do
        -- TODO:
        --   - exceptions
        --   - timeout
        --   - expiration
        started_at <- liftEffect $ nowDateTime
        val <- recovering (capDelay (3.0 # Minutes) $ fullJitterBackoff (200.0 # Milliseconds)) [ \_ _ -> pure true ] (\_ -> update_function)
        resolved_at <- liftEffect $ nowDateTime
        _ <- AVar.tryTake avar
        AVar.put (Slot { value: val, requestedAt: started_at, resolvedAt: resolved_at }) avar
        delay refresh_interval
  let
    sv = SynchedValue { avar }
  pure sv

-- | Same as @new@ but waits for the first fetch to succeed
awaitNew ∷ ∀ a. Milliseconds -> Aff a -> Aff (SynchedValue a)
awaitNew refresh_interval update_function = do
  synchedValue <- liftEffect $ new refresh_interval update_function
  _ <- read synchedValue
  pure synchedValue

read ∷ ∀ a. SynchedValue a -> Aff a
read (SynchedValue { avar }) = do
  Slot { value } <- AVar.read avar
  -- TODO check expiration time
  pure value

-- | Check if a SynchedValue is filled
isEmpty ∷ ∀  m a. MonadEffect m => SynchedValue a -> m Boolean
isEmpty (SynchedValue { avar }) = liftEffect $ isNothing <$> tryRead avar
