module J1
  ( module J1.Core
  , module J1.Top
  , module J1.Types

  , run
  , runInstructions
  ) where

import           J1.Core
import           J1.Top
import           J1.Types

import           Clash.Prelude
import           Control.DeepSeq (NFData)
import           Prelude         as P

-- | Simulation function
run :: (NFData a) => (CoreOutput -> a) -> [Input] -> [a]
run f input =
  let top = withClockReset systemClockGen systemResetGen $ (f <$>) . j1WithRam'
  in P.take (P.length input) $ simulate top input

-- | Simulation function
runInstructions :: (NFData a) => (CoreOutput -> a) -> [Instruction] -> [a]
runInstructions f = run f . P.map (\i->def{instructionRx = i})
