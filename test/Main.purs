module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(..), attempt, throwError)
import Control.Monad.Aff.AVar (AVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.Retry (RetryPolicyM, RetryStatus(RetryStatus), applyPolicy, constantDelay, defaultRetryStatus, limitRetries, recovering, retryPolicy, retrying)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Exception (error, Error)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (assert, equal, shouldEqual)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

type Log r = (avar :: AVAR, console :: CONSOLE | r)

main :: ∀ r . Eff ( console :: CONSOLE
                  , testOutput :: TESTOUTPUT
                  , avar :: AVAR
                  | r
                  ) Unit
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
      let check :: ∀ x. RetryStatus -> Maybe Unit -> Aff x Boolean
          check _ = isNothing >>> pure

          action :: ∀ x . RetryStatus -> Aff (Log x) (Maybe Unit)
          action (RetryStatus { iterNumber: n }) =
            log ((if n == zero then "\n" else "") <> show n <> ". Action failed")
              $> Nothing

          for3seconds = constantDelay (Milliseconds 200.0)
                     <> limitRetries 3

      result <- retrying for3seconds check action
      assert "Nothing expected" $ isNothing result

  suite "recovering action" do

    let recoveringAction :: ∀ x. AVar Int -> RetryStatus -> Aff (Log x) Unit
        recoveringAction actionRuns (RetryStatus { iterNumber: n }) = do
          let index = (if n == zero then "\n" else "") <> show n <> ". "
          log (index <> "Action failed")
          v <- takeVar actionRuns
          putVar (v + 1) actionRuns
          throwError (error "Error")

        expectedRetries = 5

        myRetryPolicy :: ∀ x. RetryPolicyM (Aff x)
        myRetryPolicy = constantDelay (Milliseconds 200.0)
                     <> limitRetries expectedRetries

    test "recovering results in error after check returns false" do
      actionRuns <- makeVar zero
      let lastIteration = 2
          checks :: ∀ x. Array (RetryStatus -> Error -> Aff x Boolean)
          checks = pure \(RetryStatus rs) -> const $ pure (rs.iterNumber /= lastIteration)
      result <- attempt $ recovering myRetryPolicy checks (recoveringAction actionRuns)
      assert "Failure expected" $ isLeft result
      takeVar actionRuns >>= equal (one + lastIteration)

    test "recovering retries if at least one check returns true" do
      actionRuns <- makeVar zero
      let checks :: ∀ x. Array (RetryStatus -> Error -> Aff x Boolean)
          checks = [ (pure >>> pure >>> pure) false, (pure >>> pure >>> pure) true ]
      result <- attempt $ recovering myRetryPolicy checks (recoveringAction actionRuns)
      assert "Failure expected" $ isLeft result
      takeVar actionRuns >>= equal (one + expectedRetries)

    test "recovering results in error after all attempts are exhausted" do
      actionRuns <- makeVar zero
      let checks :: ∀ x. Array (RetryStatus -> Error-> Aff x Boolean)
          checks = [ (pure >>> pure >>> pure) true ]
      result <- attempt $ recovering myRetryPolicy checks (recoveringAction actionRuns)
      assert "Failure expected" $ isLeft result
      takeVar actionRuns >>= equal (one + expectedRetries)
