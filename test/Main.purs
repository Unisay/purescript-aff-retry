module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.Retry (constantDelay, limitRetries, retrying)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), isNothing)
import Data.Time.Duration (Milliseconds(..))


main :: forall r. Eff (console :: CONSOLE | r) Unit
main = launchAff_ $ retrying everySecond3 (const $ isNothing >>> pure) (const act)
  where
  act = log "bumz" *> pure Nothing
  everySecond3 = constantDelay (Milliseconds 10.0) <> limitRetries 3000
