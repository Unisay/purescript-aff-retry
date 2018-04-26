module Control.Monad.Aff.Retry
  ( RetryStatus(..)
  , RetryPolicyM
  , constantDelay
  , defaultRetryStatus
  , retryPolicy
  , limitRetries
  , retrying
  ) where

import Prelude

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Time.Duration (Milliseconds(..))

-- | Datatype with stats about retries made thus far
data RetryStatus =
  RetryStatus
  { iterNumber      :: Int
  -- ^ Iteration number, where 0 is the first try
  , cumulativeDelay :: Milliseconds
  -- ^ Delay incurred so far from retries
  , previousDelay   :: Maybe Milliseconds
  -- ^ Latest attempt's delay. Will always be Nothing on first run.
  }

instance showRetryStatus :: Show RetryStatus where
  show (RetryStatus s) =
    "RetryStatus { "
    <> "iterNumber: "      <> show s.iterNumber      <> ", "
    <> "cumulativeDelay: " <> show s.cumulativeDelay <> ", "
    <> "previousDelay: "   <> show s.previousDelay   <> "}"

derive instance eqRetryStatus :: Eq RetryStatus

-- | A 'RetryPolicyM' is a function that takes an 'RetryStatus' and
-- possibly returns a delay in milliseconds. Iteration numbers start
-- at zero and increase by one on each retry. A *Nothing* return value from
-- the function implies we have reached the retry limit.
--
-- Please note that 'RetryPolicyM' is a 'Monoid'. You can collapse
-- multiple strategies into one using 'mappend' or '<>'. The semantics
-- of this combination are as follows:
--
-- 1. If either policy returns 'Nothing', the combined policy returns
-- 'Nothing'. This can be used to @inhibit@ after a number of retries,
-- for example.
--
-- 2. If both policies return a delay, the larger delay will be used.
-- This is quite natural when combining multiple policies to achieve a
-- certain effect.
--
-- Example:
--
-- One can easily define an exponential backoff policy with a limited
-- number of retries:
--
-- >> limitedBackoff = exponentialBackoff 50 <> limitRetries 5
--
-- Naturally, 'mempty' will retry immediately (delay 0) for an
-- unlimited number of retries, forming the identity for the 'Monoid'.

newtype RetryPolicyM m
  = RetryPolicyM (RetryStatus -> m (Maybe Milliseconds))

type RetryPolicy = ∀ m . Monad m => RetryPolicyM m

instance retryPolicySemigroup :: Monad m => Semigroup (RetryPolicyM m) where
  append (RetryPolicyM a) (RetryPolicyM b) =
    RetryPolicyM $ \n -> runMaybeT $ max <$> MaybeT (a n) <*> MaybeT (b n)
    -- ^todo: remove transformers?

instance retryPolicyMonoid :: Monad m => Monoid (RetryPolicyM m) where
    mempty = retryPolicy $ const $ Just $ Milliseconds zero

-- | Helper for making simplified policies that don't use the monadic context.
retryPolicy :: (RetryStatus -> Maybe Milliseconds) -> RetryPolicy
retryPolicy f = RetryPolicyM (pure <<< f)

-- | Retry immediately, but only up to @n@ times.
limitRetries
  :: Int
  -- ^ Maximum number of retries.
  -> RetryPolicy
limitRetries i = retryPolicy $ \(RetryStatus { iterNumber: n }) ->
  if n >= i then Nothing else (Just zero)

-- | Cconstant delay with unlimited retries
constantDelay :: Milliseconds -> RetryPolicy
constantDelay ms = retryPolicy (const (Just ms))

-- | Initial, default retry status. Exported mostly to allow user code
-- to test their handlers and retry policies. Use fields or lenses to update.
defaultRetryStatus :: RetryStatus
defaultRetryStatus = RetryStatus
  { iterNumber: zero
  , cumulativeDelay: Milliseconds zero
  , previousDelay: Nothing
  }

-- | Apply policy on status to see what the decision would be.
-- 'Nothing' implies no retry, 'Just' returns updated status.
applyPolicy
  :: ∀ fx m . MonadAff fx m
  => RetryPolicyM m
  -> RetryStatus
  -> m (Maybe RetryStatus)
applyPolicy (RetryPolicyM policy) retryStatus@(RetryStatus rs) = do
    res <- policy retryStatus
    case res of
      Just delay -> pure $ Just $ RetryStatus
        { iterNumber: rs.iterNumber + 1
        , cumulativeDelay: rs.cumulativeDelay + delay -- boundedPlus
        , previousDelay: Just delay
        }
      Nothing -> pure Nothing

-- | Apply policy and delay by its amount if it results in a retry.
-- Return updated status.
applyAndDelay
  :: ∀ fx m . MonadAff (console :: CONSOLE | fx) m
  => RetryPolicyM m
  -> RetryStatus
  -> m (Maybe RetryStatus)
applyAndDelay policy retryStatus@(RetryStatus rs) = do
    chk <- applyPolicy policy retryStatus
    case chk of
      Just retryStatus'@(RetryStatus rs') -> do
        case rs'.previousDelay of
          Nothing -> pure unit
          Just d -> do
            liftAff $ log "Before delay"
            liftAff $ delay d
            liftAff $ log "After delay"
        pure (Just retryStatus')
      Nothing -> pure Nothing


-- | Retry combinator for actions that don't raise exceptions, but
-- signal in their type the outcome has failed. Examples are the
-- 'Maybe', 'Either' and 'EitherT' monads.
--
-- Let's write a function that always fails and watch this combinator
-- retry it 5 additional times following the initial run:
--
-- >>> import Data.Maybe
-- >>> let f _ = putStrLn "Running action" >> return Nothing
-- >>> retrying def (const $ return . isNothing) f
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Nothing
--
-- Note how the latest failing result is returned after all retries
-- have been exhausted.
retrying  :: ∀ fx m b . MonadAff (console :: CONSOLE | fx) m
          => RetryPolicyM m
          -> (RetryStatus -> b -> m Boolean)
          -- ^ An action to check whether the result should be retried.
          -- If True, we delay and retry the operation.
          -> (RetryStatus -> m b)
          -- ^ Action to run
          -> m b
retrying policy chk f = go defaultRetryStatus
  where
  go s = do
    res <- f s
    chk' <- chk s res
    if chk' then do
      rs <- applyAndDelay policy s
      case rs of
        Nothing -> pure res
        Just rs' -> go rs'
        else pure res
