-- | Convenience constructors for instructions, intended for testing. Use gforth
-- for actual programs
--
-- {-# LANGUAGE NoImplicitPrelude #-}

module J1.Ops where

import           Data.Monoid ((<>))
import           J1.Types
import           Prelude     hiding (and)

-- * Base Alu

t = Alu 0 NoReturn NoAction 0 0
n = Alu 1 NoReturn NoAction 0 0
aluAdd              = Alu 2 NoReturn NoAction 0 0
aluAnd              = Alu 3 NoReturn NoAction 0 0
aluOr               = Alu 4 NoReturn NoAction 0 0
aluXor              = Alu 5 NoReturn NoAction 0 0
aluComplement       = Alu 6 NoReturn NoAction 0 0
aluEqual            = Alu 7 NoReturn NoAction 0 0
aluSignedCompare    = Alu 8 NoReturn NoAction 0 0
aluShiftRight       = Alu 9 NoReturn NoAction 0 0
aluShiftLeft        = Alu 10 NoReturn NoAction 0 0
aluTopOfReturnStack = Alu 11 NoReturn NoAction 0 0
aluMemoryAtTop      = Alu 12 NoReturn NoAction 0 0
aluIO               = Alu 13 NoReturn NoAction 0 0
aluStackSizes       = Alu 14 NoReturn NoAction 0 0
aluUnsignedCompare  = Alu 15 NoReturn NoAction 0 0

topToNext    = Alu 0 NoReturn T2N 0 0
topToReturn  = Alu 0 NoReturn T2R 0 0
nextToMemory = Alu 0 NoReturn N2MT 0 0
nextToIO     = Alu 0 NoReturn N2IO 0 0
return       = Alu 0 Return NoAction 0 0

dm1 = Alu 0 NoReturn NoAction 0 (-1)
da1 = Alu 0 NoReturn NoAction 0 1
sm1 = Alu 0 NoReturn NoAction (-1) 0
sm2 = Alu 0 NoReturn NoAction (-2) 0
sa1 = Alu 0 NoReturn NoAction 1 0

noop   = t
add    = aluAdd <> dm1
xor    = aluXor <> dm1
and    = aluAnd <> dm1
depths = topToNext <> aluStackSizes <> da1
dup    = t <> da1

removeTop = Alu 0 NoReturn NoAction 0 (-1)

dataStackSize = [depths, Literal 31, and]
dumpStackSize = dataStackSize ++ [Literal 0, nextToIO <> removeTop]
