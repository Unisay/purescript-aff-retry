module Effect.Aff.Retry
  ( RetryStatus(..)
  , RetryPolicyM(..)
  , RetryPolicy
  , constantDelay
  , defaultRetryStatus
  , applyPolicy
  , retryPolicy
  , limitRetries
  , limitRetriesByDelay
  , limitRetriesByCumulativeDelay
  , retrying
  , recovering
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (uncons)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)
import Effect.Aff (delay, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Random (randomRange)
import Math (pow)

-- | Datatype with stats about retries made thus far
newtype RetryStatus =
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
derive instance newtypeRetryStatus :: Newtype RetryStatus _

-- | A 'RetryPolicyM' is a function that takes an 'RetryStatus' and
-- | possibly returns a delay in milliseconds. Iteration numbers start
-- | at zero and increase by one on each retry. A *Nothing* return value from
-- | the function implies we have reached the retry limit.
-- |
-- | Please note that 'RetryPolicyM' is a 'Monoid'. You can collapse
-- | multiple strategies into one using 'mappend' or '<>'. The semantics
-- | of this combination are as follows:
-- |
-- | 1. If either policy returns 'Nothing', the combined policy returns
-- | 'Nothing'. This can be used to @inhibit@ after a number of retries,
-- | for example.
-- |
-- | 2. If both policies return a delay, the larger delay will be used.
-- | This is quite natural when combining multiple policies to achieve a
-- | certain effect.
-- |
-- | Example:
-- |
-- | One can easily define an exponential backoff policy with a limited
-- | number of retries:
-- |
-- >> limitedBackoff = exponentialBackoff 50 <> limitRetries 5
-- |
-- | Naturally, 'mempty' will retry immediately (delay 0) for an
-- | unlimited number of retries, forming the identity for the 'Monoid'.

newtype RetryPolicyM m
  = RetryPolicyM (RetryStatus -> m (Maybe Milliseconds))

type RetryPolicy = ∀ m . MonadAff m => RetryPolicyM m

instance retryPolicySemigroup :: Monad m => Semigroup (RetryPolicyM m) where
  append (RetryPolicyM a) (RetryPolicyM b) =
    RetryPolicyM $ \n -> runMaybeT $ max <$> MaybeT (a n) <*> MaybeT (b n)

instance retryPolicyMonoid :: MonadAff m => Monoid (RetryPolicyM m) where
    mempty = retryPolicy $ const $ Just $ Milliseconds zero

-- | Helper for making simplified policies that don't use the monadic context.
retryPolicy :: (RetryStatus -> Maybe Milliseconds) -> RetryPolicy
retryPolicy f = RetryPolicyM (pure <<< f)

-- | Retry immediately, but only up to @n@ times.
limitRetries
  :: Int -- Maximum number of retries.
  -> RetryPolicy
limitRetries i = retryPolicy $ \(RetryStatus { iterNumber: n }) ->
  if n >= i then Nothing else Just mempty

-- | Add an upperbound to a policy such that once the given time-delay
-- | amount *per try* has been reached or exceeded, the policy will stop
-- | retrying and fail. If you need to stop retrying once *cumulative*
-- | delay reaches a time-delay amount, use 'limitRetriesByCumulativeDelay'
limitRetriesByDelay
  :: ∀ d m . Monad m => Duration d => d -> RetryPolicyM m -> RetryPolicyM m
limitRetriesByDelay d (RetryPolicyM policy) =
  RetryPolicyM \status -> bindFlipped limit <$> policy status
  where limit delay = if delay >= fromDuration d
                      then Nothing
                      else Just delay

-- | Add an upperbound to a policy such that once the cumulative delay
-- | over all retries has reached or exceeded the given limit, the
-- | policy will stop retrying and fail.
limitRetriesByCumulativeDelay
  :: ∀ d m . Monad m => Duration d => d -> RetryPolicyM m -> RetryPolicyM m
limitRetriesByCumulativeDelay d (RetryPolicyM policy) =
  RetryPolicyM \status -> bindFlipped (limit status) <$> policy status
  where
    limit (RetryStatus rs) curDelay
      | (rs.cumulativeDelay <> curDelay) > cumulativeDelay = Nothing
      | otherwise = Just curDelay
    cumulativeDelay = fromDuration d

-- | Cconstant delay with unlimited retries
constantDelay :: ∀ d . Duration d => d -> RetryPolicy
constantDelay d = retryPolicy <<< const <<< pure <<< fromDuration $ d

-- | Set a time-upperbound for any delays that may be directed by the
-- | given policy.  This function does not terminate the retrying. The policy
-- | `capDelay maxDelay (exponentialBackoff n)` will never stop retrying.  It
-- | will reach a state where it retries forever with a delay of `maxDelay`
-- | between each one. To get termination you need to use one of the
-- | 'limitRetries' function variants.
capDelay
  :: ∀ d m . Monad m => Duration d => d -> RetryPolicyM m -> RetryPolicyM m
capDelay limit (RetryPolicyM policy) =
  RetryPolicyM \status -> map (min (fromDuration limit)) <$> policy status

-- | Grow delay exponentially each iteration.
-- | Each delay will increase by a factor of two.
exponentialBackoff :: ∀ d . Duration d => d -> RetryPolicy
exponentialBackoff base = retryPolicy \(RetryStatus { iterNumber: n }) ->
  Just $ Milliseconds $ unwrap (fromDuration base) * pow 2.0 (toNumber n)

fibonacciBackoff :: ∀ d . Duration d => d -> RetryPolicy
fibonacciBackoff duration = retryPolicy \(RetryStatus { iterNumber: n }) ->
  Just $ Milliseconds $ fib ((toNumber n) + one) { a: zero, b: base }
    where
      (Milliseconds base) = fromDuration duration
      fib 0.0 { a, b } = a
      fib m   { a, b } = fib (m - one) { a: b, b: a + b }

-- | FullJitter exponential backoff as explained in AWS Architecture Blog article.
-- | @http:\/\/www.awsarchitectureblog.com\/2015\/03\/backoff.html@
-- | temp = min(cap, base * 2 ** attempt)
-- | sleep = temp \/ 2 + random_between(0, temp \/ 2)
fullJitterBackoff
  :: ∀ m d
   . MonadAff m
  => Duration d
  => d
  -> RetryPolicyM m
fullJitterBackoff duration = RetryPolicyM \(RetryStatus { iterNumber: n }) -> do
  let (Milliseconds ms) = fromDuration duration
      d = (ms * pow 2.0 (toNumber n)) `div` 2.0
  rand <- liftEffect $ randomRange zero d
  pure $ Just $ Milliseconds $ d + rand

-- | Initial, default retry status. Exported mostly to allow user code
-- | to test their handlers and retry policies. Use fields or lenses to update.
defaultRetryStatus :: RetryStatus
defaultRetryStatus = RetryStatus
  { iterNumber: zero
  , cumulativeDelay: Milliseconds zero
  , previousDelay: Nothing
  }

-- | Apply policy on status to see what the decision would be.
-- | 'Nothing' implies no retry, 'Just' returns updated status.
applyPolicy
  :: ∀ m . MonadAff m
  => RetryPolicyM m
  -> RetryStatus
  -> m (Maybe RetryStatus)
applyPolicy (RetryPolicyM policy) retryStatus@(RetryStatus rs) = do
  res <- policy retryStatus
  case res of
    Just delay -> pure $ Just $ RetryStatus
      { iterNumber: rs.iterNumber + one
      , cumulativeDelay: rs.cumulativeDelay <> delay -- boundedPlus? no
      , previousDelay: Just delay
      }
    Nothing -> pure Nothing

-- | Apply policy and delay by its amount if it results in a retry.
-- | Returns updated status.
applyAndDelay :: ∀ m . MonadAff m
  => RetryPolicyM m
  -> RetryStatus
  -> m (Maybe RetryStatus)
applyAndDelay policy retryStatus = do
    res <- applyPolicy policy retryStatus
    case res of
      Just nextRetryStatus@(RetryStatus { previousDelay: pd }) ->
        maybe (pure unit) (delay >>> liftAff) pd
          $> Just nextRetryStatus
      Nothing -> pure Nothing


-- | Retry combinator for actions that don't raise exceptions, but
-- | signal in their type the outcome has failed. Examples are the
-- | 'Maybe', 'Either' and 'EitherT' monads.
retrying :: ∀ m b . MonadAff m
  => RetryPolicyM m
  -> (RetryStatus -> b -> m Boolean) -- An action to check whether the result should be retried.
                                     -- If True, we delay and retry the operation.
  -> (RetryStatus -> m b)            -- Action to run
  -> m b
retrying policy check action = go defaultRetryStatus
  where
  go status = do
    res <- action status
    ifM (check status res)
      (applyAndDelay policy status >>= maybe (pure res) go)
      (pure res)

-- | Run an action and recover from a raised exception by potentially
-- | retrying the action a number of times.
recovering :: ∀ m a . MonadAff m
  => MonadError Error m
  => RetryPolicyM m
  -> Array (RetryStatus -> Error -> m Boolean)
  -- ^ Should a given exception be retried? Action will be
  -- retried if this returns True *and* the policy allows it.
  -- This action will be consulted first even if the policy
  -- later blocks it.
  -> (RetryStatus -> m a) -- Action to perform
  -> m a
recovering policy checks f = go defaultRetryStatus
  where
  go status = try (f status) >>= either (recover checks) pure
    where
    recover chks e = uncons chks # maybe (throwError e) (handle e)
    handle e hs =
      ifM (hs.head status e)
        (applyAndDelay policy status >>= maybe (throwError e) go)
        (recover hs.tail e)
