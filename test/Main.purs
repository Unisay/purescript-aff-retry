module Test.Main where

import Prelude

import Data.Array as A
import Data.Either (isLeft)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, throwError)
import Effect.Aff.AVar (AVar, new, put, take)
import Effect.Aff.Retry (RetryPolicyM, RetryStatus(RetryStatus), applyPolicy, constantDelay, defaultRetryStatus, limitRetries, limitRetriesByCumulativeDelay, recovering, retryPolicy, retrying)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, Error)
import Test.Retry (simulatePolicy)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (assert, equal, shouldEqual)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do

  suite "retry policy" do

    test "if either policy returns Nothing then composition also returns Nothing" do
      let policy1 = retryPolicy \_ -> Just $ Milliseconds zero
          policy2 = retryPolicy \_ -> Nothing
      status <- applyPolicy (policy1 <> policy2) defaultRetryStatus
      assert "Expected Nothing" $ isNothing status

    test "if both policies return a delay, larger delay is used" do
      let policy1 = retryPolicy \_ -> Just $ Milliseconds zero
          policy2 = retryPolicy \_ -> Just $ Milliseconds one
      mbStatus <- applyPolicy (policy1 <> policy2) defaultRetryStatus
      mbStatus # maybe (failure "Just expected") \(RetryStatus { previousDelay: delay }) ->
        delay `shouldEqual` Just (Milliseconds one)

  suite "retry action" do

    test "retrying results in error after all attempts are exhausted" do
      let check :: RetryStatus -> Maybe Unit -> Aff Boolean
          check _ = isNothing >>> pure

          action :: RetryStatus -> Aff (Maybe Unit)
          action (RetryStatus { iterNumber: n }) =
            liftEffect $ log ((if n == zero then "\n" else "") <> show n <> ". Action failed")
              $> Nothing

          for3seconds = constantDelay (Milliseconds 200.0)
                     <> limitRetries 3

      result <- retrying for3seconds check action
      assert "Nothing expected" $ isNothing result

  suite "recovering action" do

    let recoveringAction :: AVar Int -> RetryStatus -> Aff Unit
        recoveringAction actionRuns (RetryStatus { iterNumber: n }) = do
          let index = (if n == zero then "\n" else "") <> show n <> ". "
          liftEffect $ log (index <> "Action failed")
          v <- take actionRuns
          put (v + 1) actionRuns
          throwError (error "Error")

        expectedRetries = 5

        myRetryPolicy :: RetryPolicyM Aff
        myRetryPolicy = constantDelay (Milliseconds 200.0)
                     <> limitRetries expectedRetries

    test "recovering results in error after check returns false" do
      actionRuns <- new zero
      let lastIteration = 2
          checks :: Array (RetryStatus -> Error -> Aff Boolean)
          checks = pure \(RetryStatus rs) -> const $ pure (rs.iterNumber /= lastIteration)
      result <- attempt $ recovering myRetryPolicy checks (recoveringAction actionRuns)
      assert "Failure expected" $ isLeft result
      take actionRuns >>= equal (one + lastIteration)

    test "recovering retries if at least one check returns true" do
      actionRuns <- new zero
      let checks :: Array (RetryStatus -> Error -> Aff Boolean)
          checks = [ (pure >>> pure >>> pure) false, (pure >>> pure >>> pure) true ]
      result <- attempt $ recovering myRetryPolicy checks (recoveringAction actionRuns)
      assert "Failure expected" $ isLeft result
      take actionRuns >>= equal (one + expectedRetries)

    test "recovering results in error after all attempts are exhausted" do
      actionRuns <- new zero
      let checks :: Array (RetryStatus -> Error-> Aff Boolean)
          checks = [ (pure >>> pure >>> pure) true ]
      result <- attempt $ recovering myRetryPolicy checks (recoveringAction actionRuns)
      assert "Failure expected" $ isLeft result
      take actionRuns >>= equal (one + expectedRetries)

    test "cumulative delays don't exceed given limit" do
      let baseDelay = Milliseconds 90.0
          cumulativeDelayMax = Milliseconds 30.0
          policy = limitRetriesByCumulativeDelay cumulativeDelayMax myRetryPolicy
      results <- simulatePolicy 100 policy
      let delays = A.catMaybes (snd <$> results)
          actualCumulativeDelay =
            Milliseconds $ sum ((\(Milliseconds ms) -> ms) <$> delays)
      assert "Actual cumulative delay is less than max delay"
        (actualCumulativeDelay <= cumulativeDelayMax)
