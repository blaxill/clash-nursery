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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module J1.Types where

import           Clash.Prelude
import           GHC.Generics

-- * Shorthand type aliases

type Word16 = BitVector 16
type Word13 = BitVector 13
type StackDepth = 32
type StackIndex = BitVector (CLog 2 StackDepth)

-- * Instruction types

-- | ALU operation to perform, see the source for 'alu' for the available
-- operations.
newtype AluOp = AluOp { unaluOp :: BitVector 4 }
  deriving (Show, Eq, Enum, Num)

-- | Encodes if the instruction N, R, [T] or IO is overwritten by T or N.
data InstructionAction
  = NoAction
  | T2N  -- ^ T -> N (Writes top to next of data stack)
  | T2R  -- ^ T -> R (Writes top to top of return stack)
  | N2MT -- ^ N -> [T] (Writes next of data stack to memory at "top of data stack" address)
  | N2IO -- ^ N -> IO (Writes next of stack to the IO port)
  deriving (Show, Eq)

-- | 'Alu' flag designating if the top of return stack will pop and overwrite
-- the program counter.
data Return
  = Return | NoReturn
  deriving (Show, Eq)

-- | Instruction data type, either a literal, a jump/call or an ALU operation.
data Instruction
  = Literal (BitVector 15) -- ^ 15 bit literal
  | Jump Word13            -- ^ 13 bit absolute jump
  | ConditionalJump Word13 -- ^ 13 bit absolute conditional jump
  | Call Word13            -- ^ 13 bit absolute jump, and push the PC + 1 to the return stack
  | Alu
    AluOp
    Return
    InstructionAction
    (Signed 2)
    (Signed 2) -- ^ Alu operation containing 'AluOp', 'Return', 'InstructionAction',
               -- return stack adjustment ('Signed' 2) and data stack adjustment ('Signed' 2)
  deriving (Show, Eq)

instance BitPack AluOp where
  type BitSize AluOp = 4
  pack = pack
  unpack = unpack

instance BitPack Return where
  type BitSize Return = 1
  pack Return   = 1
  pack NoReturn = 0
  unpack 1 = Return
  unpack 0 = NoReturn

instance BitPack InstructionAction  where
  type BitSize InstructionAction = 3
  pack NoAction = 0
  pack T2N      = 1
  pack T2R      = 2
  pack N2MT     = 3
  pack N2IO     = 4
  unpack 0 = NoAction
  unpack 1 = T2N
  unpack 2 = T2R
  unpack 3 = N2MT
  unpack 4 = N2IO
  unpack _ = errorX "Error decoding instruction flags!"

instance BitPack Instruction where
  type BitSize Instruction = 16
  pack (Literal v)         = 0b1 ++# v
  pack (Jump v)            = 0b000 ++# v
  pack (ConditionalJump v) = 0b001 ++# v
  pack (Call v)            = 0b010 ++# v
  pack (Alu op r2pc flags rpd dpd) =
    (0b0110 :: BitVector 4) ++# pack (op,r2pc,flags,rpd,dpd)
  unpack bits =
    let hibits = slice d15 d13 bits
        bits14 = slice d14 d0 bits
        bits12 = slice d12 d0 bits
        bits11 = slice d11 d0 bits
    in case bv2v hibits of
      1:>_:>_:>Nil -> Literal bits14
      0:>0:>0:>Nil -> Jump bits12
      0:>1:>0:>Nil -> ConditionalJump bits12
      0:>0:>1:>Nil -> Call bits12
      0:>1:>1:>Nil -> let (a,b,flags,g,h) = unpack bits11
                      in Alu a b flags g h
      _            -> errorX "Failed to decode instruction!"

-- * Core types

data WriteAddr = WriteAddr
  { writeAddr  :: Word16
  , writeValue :: Word16
  } deriving (Show, Eq)

data StackPointers p = StackPointers
  { dataPointer   :: p
  , returnPointer :: p
  } deriving (Show, Eq, Generic)

data State = State
  { programCounter :: Word13
  , dataStack0     :: Word16
  , stackPointers  :: StackPointers StackIndex
  } deriving (Show, Eq, Generic)

data Input = Input
  { memoryRx      :: Word16
  , ioRx          :: Word16
  , instructionRx :: Word16
  } deriving (Show, Eq, Generic)

data CoreInput = CoreInput
  { input           :: Input
  , dataStackRead   :: Word16
  , returnStackRead :: Word16
  } deriving (Show, Eq, Generic)

data CoreOutput = CoreOutput
  { memoryReadAddr   :: Word16
  , memoryTx         :: Maybe WriteAddr
  , ioTx             :: Maybe Word16
  , instructionAddr  :: Word13
  , dataStackWrite   :: Maybe Word16
  , returnStackWrite :: Maybe Word16
  , setStackPointers :: StackPointers StackIndex
  } deriving (Show, Eq, Generic)

instance Bundle (StackPointers p) where
  type Unbundled domain (StackPointers p) = StackPointers (Signal domain p)
  bundle (StackPointers dp rp) = StackPointers <$> dp <*> rp
  unbundle p      = StackPointers (dataPointer <$> p) (returnPointer <$> p)
instance ShowX p => ShowX (StackPointers p)
instance Default p => Default (StackPointers p)

instance ShowX State
instance Default State
