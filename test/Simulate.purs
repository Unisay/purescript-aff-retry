module Test.Retry where

import Prelude

import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array as A
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Retry (RetryPolicyM(..), defaultRetryStatus, RetryStatus(..))

simulatePolicy
  :: ∀ m
   . Monad m
  => Int
  -> RetryPolicyM m
  -> m (Array (Tuple Int (Maybe Milliseconds)))
simulatePolicy n (RetryPolicyM f)
  = flip evalStateT defaultRetryStatus $ for (A.range 0 n) $ \i -> do
  (RetryStatus stat) <- get
  delay <- lift (f (RetryStatus stat))
  put (RetryStatus (stat
    { iterNumber = i + 1
    , cumulativeDelay = stat.cumulativeDelay  <>
                        fromMaybe (Milliseconds zero) delay
    , previousDelay = delay
    }))
  pure $ Tuple i delay
