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
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module J1.Core
  ( alu
  , core
  ) where

import           Clash.Prelude
import           J1.Types

-- | "Decodes" and executes the 'AluOp', returning the new data stack value.
-- This does not control data/return stack depth changes, which are encoded in
-- the instructions themselves.
alu :: AluOp
    -> Word16      -- ^ Top of data stack
    -> Word16      -- ^ Next of data stack
    -> Word16      -- ^ Top of return stack
    -> Word16      -- ^ Memory read at T
    -> Word16      -- ^ Memory read at T
    -> BitVector 5 -- ^ Return stack depth
    -> BitVector 5 -- ^ Data stack depth
    -> Word16      -- ^ New top of stack
alu o t n tr mt io rd dd = case o of
    0  -> t
    1  -> n
    2  -> t + n
    3  -> t .&. n
    4  -> t .|. n
    5  -> t `xor` n
    6  -> complement t
    7  -> boolExpand $ n == t
    8  -> boolExpand $ unpack @(Signed _) n < unpack @(Signed _) t
    9  -> shiftR n (fromEnum $ slice d4 d0 t)
    10 -> shiftL n (fromEnum $ slice d4 d0 t)
    11 -> tr
    12 -> mt
    13 -> io
    14 -> resize (pack rd ++# pack dd)
    15 -> boolExpand $ n < t
    _  -> errorX "Unrecognized ALU operation!"
  where
    boolExpand :: (KnownNat n) => Bool -> BitVector n
    boolExpand True  = maxBound
    boolExpand False = minBound

-- | Core J1 'mealy' transfer function.
core :: State -> CoreInput -> (State, CoreOutput)
core s i =
  let -- Shorthand variables
      t = dataStack0 s
      n = dataStackRead i
      tr = returnStackRead i
      mt = memoryRx (input i)
      io = ioRx (input i)
      dp = dataPointer (stackPointers s)
      rp = returnPointer (stackPointers s)
      pc = programCounter s

      pc1 = pc + 1

      -- Instruction decoding and execution
      instruction :: Instruction
      instruction = instructionRx $ input i
      (pc', t', action, pc2r, rpd, dpd) = case instruction of
        Literal v                        ->
          (pc1, 0 ++# v, T2N, False, 0, 1)
        Jump addr                        ->
          (addr, t, NoAction, False, 0, 0)
        ConditionalJump addr             ->
          if t == 0
          then (addr, n, NoAction, False, 0, -1)
          else (pc1, n, NoAction, False, 0, -1)
        Call addr                        ->
          (addr, t, NoAction, True, 1, 0)
        Alu op r2pc action rpd dpd ->
          -- Run alu
          let t' = alu op t n tr mt io rp dp
              pc' = case r2pc of
                      Return   -> slice d13 d1 tr
                      NoReturn -> pc1
          in (pc', t', action, False, rpd, dpd)

      -- Update stack pointers
      dp' = dp + signExtend (pack dpd)
      rp' = rp + signExtend (pack rpd)
  in (State
        { programCounter  = pc'
        , dataStack0      = t'
        , stackPointers   = StackPointers
          { dataPointer   = dp'
          , returnPointer = rp' }
        }
      -- Dispatch all writes
     , CoreOutput
        { memoryReadAddr   = t'
        , tx               = WriteAddr t n
        , memoryTxEnable   = action == N2MT
        , ioTxEnable       = action == N2IO
        , instructionAddr  = pc'
        , returnStackWrite = case (pc2r, action == T2R) of
              -- An invalid instruction can possibly have both the PC -> R bit
              -- and T -> R value set, which we cannot process.
              (True, True) -> errorX "Bad instruction, both the PC -> R bit and T -> R value are set."
              (True, _)    -> Just (0 ++# pc1 ++# (0 :: BitVector 1))
              (_, True)    -> Just t
              (_, _)       -> Nothing
        , dataStackWrite   = if action == T2N
                             then Just t
                             else Nothing
        , setStackPointers = StackPointers
          { dataPointer    = dp'
          , returnPointer  = rp' }
        })
