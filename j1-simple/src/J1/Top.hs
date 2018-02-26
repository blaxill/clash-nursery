{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

{-# LANGUAGE BinaryLiterals            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module J1.Top
  ( j1'
  , j1
  , j1WithRam'
  , j1WithRam
  ) where

import qualified Clash.Explicit.BlockRam as E
import qualified Clash.Explicit.Signal   as E (register)
import           Clash.Prelude
import           GHC.Generics
import           J1.Core
import           J1.Types
import qualified Prelude                 as P

j1' :: (HasClockReset domain gated synchronous)
  => Signal domain CoreInput -> Signal domain CoreOutput
j1' = mealy core def

{-# ANN j1 defTop{t_name="j1"} #-}
-- | J1 core top entity specialized to 'System' clock with a 'Synchronous' reset.
j1 :: Clock System 'Source
   -> Reset System 'Synchronous
   -> Signal System CoreInput
   -> Signal System CoreOutput
j1 clk rst input = withClockReset clk rst (j1' input)
{-# NOINLINE j1 #-}

j1WithRam' :: (HasClockReset domain gated synchronous)
  => Signal domain Input
  -> Signal domain CoreOutput
j1WithRam' input =
    let output = j1' (CoreInput <$> input <*> rsRead <*> dsRead)

        (StackPointers dp rp) = unbundle (setStackPointers <$> output)
        dpu = fmap unpack dp
        rpu = fmap unpack rp
        dw = dataStackWrite <$> output
        rw = returnStackWrite <$> output

        dataStack = J1.Top.readNew (blockRamPow2 $
          repeat (errorX "Uninitialized data stack element!"))
        returnStack = J1.Top.readNew (blockRamPow2 $
          repeat (errorX "Uninitialized return stack element!"))

        dsRead = dataStack dpu (tagWriteWithPointer <$> dpu <*> dw)
        rsRead = returnStack rpu (tagWriteWithPointer <$> rpu <*> rw)
    in output
  where
      tagWriteWithPointer addr (Just v) = Just (addr, v)
      tagWriteWithPointer _ _           = Nothing

{-# ANN j1WithRam defTop{t_name="j1WithRam"} #-}
-- | J1 top entity with dual port ram stacks specialized to 'System' clock with
-- a 'Synchronous' reset.
j1WithRam :: Clock System 'Source
  -> Reset System 'Synchronous
  -> Signal System Input
  -> Signal System CoreOutput
j1WithRam clk rst input = withClockReset clk rst $
    j1WithRam' input
{-# NOINLINE j1WithRam #-}

-- * Misc

-- | Merged version of the implicit and explicit Clash 'Clash.Prelude.readNew' function
-- to prevent metastability warnings.
readNew :: (Eq addr, HasClockReset domain gated synchronous)
        => (Signal domain addr -> Signal domain (Maybe (addr, a)) -> Signal domain a)
        -> Signal domain addr
        -> Signal domain (Maybe (addr, a))
        -> Signal domain a
readNew ram rdAddr wrM = mux wasSame wasWritten $ ram rdAddr wrM
  where readNewT rd (Just (wr, wrdata)) = (wr == rd, wrdata)
        readNewT _  Nothing             = (False   , undefined)

        (wasSame,wasWritten) =
          unbundle (register (False,undefined)
                              (readNewT <$> rdAddr <*> wrM))
{-# INLINE readNew #-}
