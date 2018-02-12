{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE UndecidableInstances       #-}

module J1.Core where

import qualified Clash.Explicit.BlockRam as E
import qualified Clash.Explicit.Signal   as E (register)
import           Clash.Prelude
import           GHC.Generics
import qualified Prelude                 as P

type Word16 = BitVector 16
type StackDepth = 32
type StackIndex = BitVector (CLog 2 StackDepth)

newtype AluOp = AluOp { unaluOp :: BitVector 4 }
  deriving (Show, Eq, Enum, Num)

instance BitPack AluOp where
  type BitSize AluOp = 4
  pack = pack
  unpack = unpack

data Instruction = Literal (BitVector 15)
                 | Jump (BitVector 13)
                 | ConditionalJump (BitVector 13)
                 | Call (BitVector 13)
                 | Alu
                   Bool       -- ^ R -> PC
                   AluOp
                   Bool       -- ^ T -> N
                   Bool       -- ^ T -> R
                   Bool       -- ^ N -> [T]
                   (Signed 2) -- Return stack pointer delta
                   (Signed 2) -- Data stack pointer delta
  deriving (Show, Eq)

instance BitPack Instruction where
  type BitSize Instruction = 16
  pack (Literal v)         = 0b1 ++# v
  pack (Jump v)            = 0b000 ++# v
  pack (ConditionalJump v) = 0b001 ++# v
  pack (Call v)            = 0b010 ++# v
  pack (Alu a b c d e f g) = 0b011 ++# pack (a,b,c,d,e,0::BitVector 1,f,g)
  unpack bits =
    let bits14 = slice d14 d0 bits
        bits12 = slice d12 d0 bits
    in case (bv2v . slice d15 d13) bits of
      1:>_:>_:>Nil -> Literal bits14
      0:>0:>0:>Nil -> Jump bits12
      0:>1:>0:>Nil -> ConditionalJump bits12
      0:>0:>1:>Nil -> Call bits12
      0:>1:>1:>Nil -> let (a,b,c,d,e,_::BitVector 1,f,g) = unpack bits12
                      in Alu a b c d e f g
      _            -> errorX "Failed to decode instruction!"

data StackPointers = StackPointers
  { dataPointer   :: StackIndex
  , returnPointer :: StackIndex
  } deriving (Show, Eq, Generic)

instance Bundle StackPointers where
  type Unbundled t StackPointers = (Signal t StackIndex, Signal t StackIndex)
  bundle (dp, rp) = StackPointers <$> dp <*> rp
  unbundle p      = (fmap dataPointer p, fmap returnPointer p)
instance ShowX StackPointers
instance Default StackPointers

data State = State
  { programCounter :: BitVector 13
  , dataStack0     :: Word16
  , stackPointers  :: StackPointers
  } deriving (Show, Eq, Generic)

instance ShowX State
instance Default State

data Input = Input
  { memoryRx        :: Word16
  , instructionRx   :: Word16
  , dataStackRead   :: Word16
  , returnStackRead :: BitVector 13
  } deriving (Show, Eq, Generic)

data MemoryWrite = NoMemoryWrite
                 | Write
                 { writeAddr  :: Word16
                 , writeValue :: Word16 }
  deriving (Show, Eq)

data Output = Output
  { memoryReadAddr   :: Word16
  , memoryTx         :: MemoryWrite
  , instructionAddr  :: BitVector 13
  , dataStackWrite   :: Maybe Word16
  , returnStackWrite :: Maybe (BitVector 13)
  , setStackPointers :: StackPointers
  } deriving (Show, Eq, Generic)

boolExpand :: (KnownNat n) => Bool -> BitVector n
boolExpand True  = maxBound
boolExpand False = minBound

alu :: AluOp
    -> Word16     -- ^ Top of data stack
    -> Word16     -- ^ Next of data stack
    -> BitVector 13     -- ^ Top of return stack
    -> Word16     -- ^ Memory read at T
    -> BitVector 5 -- ^ Data stack depth
    -> BitVector 5 -- ^ Return stack depth
    -> Word16     -- ^ Out
alu o t n tr mt dd rd = case o of
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
  11 -> resize tr
  12 -> mt
  --13 -> _ -- io_in?
  14 -> resize (pack rd ++# pack dd)
  15 -> boolExpand $ n < t
  _  -> errorX "Unrecognized ALU operation!"

core :: State -> Input -> (State, Output)
core s i =
  let t = dataStack0 s
      n = dataStackRead i
      tr = returnStackRead i
      mt = memoryRx i
      dp = dataPointer (stackPointers s)
      rp = returnPointer (stackPointers s)
      pc = programCounter s

      pc1 = pc + 1

      instruction :: Instruction
      instruction = unpack (instructionRx i)

      (pc', t', t2n, t2r, n2mt, pc2r, rpd, dpd) = case instruction of
        Literal v                        ->
          (pc1, 0 ++# v, True, False, False, False, 0, 1)
        Jump addr                        ->
          (addr, t, False, False, False, False, 0, 0)
        ConditionalJump addr             ->
          if t /= 0
          then (addr, n, False, False, False, False, 0, -1)
          else (pc1, n, False, False, False, False, 0, -1)
        Call addr                        ->
          (addr, t, False, False, False, True, 1, 0)
        Alu r2pc op t2n t2r n2mt rpd dpd ->
          let t' = alu op t n tr mt dp rp
          in (if r2pc then tr else pc1, t', t2n, t2r, n2mt, False, rpd, dpd)

      dp' = dp + signExtend (pack dpd)
      rp' = rp + signExtend (pack rpd)

      o = Output
        { memoryReadAddr   = t'
        , memoryTx         = if n2mt
            then Write {writeAddr = t, writeValue = n}
            else NoMemoryWrite
        , instructionAddr  = pc'
        , returnStackWrite = case (pc2r, t2r) of
            (True, True) -> errorX "This is a bug, please submit a report!"
            (True, _)    -> Just pc1
            (_, True)    -> Just (slice d12 d0 t)
            (_, _)       -> Nothing
        , dataStackWrite   = if t2n
            then Just t
            else Nothing
        , setStackPointers = StackPointers
          { dataPointer   = dp'
          , returnPointer = rp' }
        }

      s' = State
        { programCounter = pc'
        , dataStack0     = t'
        , stackPointers = StackPointers
          { dataPointer   = dp'
          , returnPointer = rp' }
        }
  in (s', o)

j1' :: (HasClockReset domain gated synchronous)
  => Signal domain Input -> Signal domain Output
j1' input = mealy core def input

{-# ANN j1 defTop{t_name="j1"} #-}
j1 :: Clock System 'Source
   -> Reset System 'Asynchronous
   -> Signal System Input
   -> Signal System Output
j1 clk rst input = withClockReset clk rst (j1' input)
{-# NOINLINE j1 #-}

-- | Merged the implicit and explicit Clash readnew function
-- to prevent metastability warnings
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

j1WithRam' :: (HasClockReset domain gated synchronous)
  => Signal domain Word16
  -> Signal domain Word16
  -> Signal domain Output
j1WithRam' memoryRead instructionRead = out
  where
    input = Input <$> memoryRead <*> instructionRead <*> dsRead <*> rsRead

    out = j1' input

    (dp, rp) = unbundle (setStackPointers <$> out)
    dpu = fmap unpack dp
    rpu = fmap unpack rp
    dw = dataStackWrite <$> out
    rw = returnStackWrite <$> out

    tagWriteWithPointer addr (Just v) = Just (addr, v)
    tagWriteWithPointer _ _           = Nothing

    dataStack = J1.Core.readNew (blockRamPow2 $
      repeat (errorX "Uninitialized stack element!"))
    returnStack = J1.Core.readNew (blockRamPow2 $
      repeat (errorX "Uninitialized stack element!"))

    dsRead = dataStack dpu (tagWriteWithPointer <$> dpu <*> dw)
    rsRead = returnStack rpu (tagWriteWithPointer <$> rpu <*> rw)

{-# ANN j1WithRam defTop{t_name="j1WithRam"} #-}
j1WithRam :: Clock System 'Source
  -> Reset System 'Asynchronous
  -> Signal System Word16 -- ^ memory read
  -> Signal System Word16 -- ^ instruction read
  -> Signal System Output
j1WithRam clk rst memoryRead instructionRead = withClockReset clk rst $
    j1WithRam' memoryRead instructionRead
{-# NOINLINE j1WithRam #-}
