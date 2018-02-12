-- | Single message block SHA-256 core.

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module HashCores.Sha256 (
  Cell,
  ShaState,
  Hash,
  Message,
  Schedule,
  initial,
  kRom,
  step,
  finalize,
  naiveSingleBlockPipe,
  naiveSingleBlockPipe',
  preprocessI,
  preprocess
  )
  where

import           Clash.DelayedExtra
import           Clash.Prelude

import qualified Data.Char          as C (ord)
import qualified Prelude            as P

-- | Short hand for a 32 bit 'BitVector'
type Cell = BitVector 32
-- | Internal SHA-256 state
type ShaState = Vec 8 Cell
-- | Output hash value
type Hash = Vec 8 Cell
-- | A single message block
type Message = Vec 16 Cell
-- | Internal SHA-256 message scheduling array
type Schedule = Vec 16 Cell

-- | SHA-256 initial state
initial :: ShaState
initial = 0x6a09e667 :>
          0xbb67ae85 :>
          0x3c6ef372 :>
          0xa54ff53a :>
          0x510e527f :>
          0x9b05688c :>
          0x1f83d9ab :>
          0x5be0cd19 :> Nil

-- | SHA-256 round constants
kRom :: Enum addr => addr -> BitVector 32
kRom = asyncRom $(listToVecTH [
      0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
      0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
      0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
      0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
      0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
      0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
      0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
      0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
      0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
      0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
      0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
      0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
      0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
      0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
      0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
      0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
      :: BitVector 32])

-- | Pipelined version of the main compression routine along with the scheduling
-- routine for w. Using DSignals allows us to pipeline and have the compiler
-- typecheck our routing delays.
step :: forall domain gated synchronous d.
      (HasClockReset domain gated synchronous, KnownNat d)
      => DSignal domain d Cell           -- ^ K constant for this round
      -> DSignal domain d Schedule       -- ^ W schedule array with the current
                                         -- value at the head
      -> DSignal domain d ShaState       -- ^ Current state
      -> (DSignal domain (d+2) Schedule,
          DSignal domain (d+2) ShaState) -- ^ Delayed output (schedule, state)
step k w state = (w', twist <$> e' <*> a' <*> delayDI state)
  where
    compression :: ShaState -> Vec 4 Cell
    compression (a:>b:>c:>d:>e:>f:>g:>h:>Nil) =
      let s1  = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
          s0  = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
          ch  = (e .&. f) `xor` (complement e .&. g)
          maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
       in s1 :> s0 :> ch :> maj :> Nil

    scheduling :: Schedule -> Vec 4 Cell
    scheduling (w0:>w1:>_:>_:>_:>_:>_  :>_:>
                _ :>w9:>_:>_:>_:>_:>w14:>_:>Nil) =
      let s0  = rotateR w1  7  `xor` rotateR w1  18 `xor` shiftR w1  3
          s1  = rotateR w14 17 `xor` rotateR w14 19 `xor` shiftR w14 10
       in w9 :> w0 :> s0 :> s1 :> Nil

    twist e a = replace 4 e . (+>>) a
    (s1 :> s0 :> ch :> maj :> Nil) = unbundleD (compression <$> state)

    stateIx ix = (!!) <$> state <*> ix
    d = delayDI (stateIx 3)
    h = (stateIx 7)

    temp1 = pipelineFold (+) (h :> s1 :> ch :> k + fmap head w :> Nil)
    temp2 = delayDI (s0 + maj)
    a' = (temp1+temp2)
    e' = d + temp1

    w' = (<<+) <$> delayDI w
               <*> pipelineFold (+) (unbundleD $ scheduling <$> w)

-- | SHA-256 round finalizer.
finalize :: DSignal domain d ShaState -> DSignal domain d ShaState
finalize v = zipWith (+) initial <$> v

-- | A pipelined SHA-256 core which only accepts single message-block inputs.
naiveSingleBlockPipe :: (HasClockReset domain gated synchronous, KnownNat d)
           => DSignal domain d Message
           -> DSignal domain (d+128) ShaState
naiveSingleBlockPipe =
     finalize' .
     -- https://github.com/clash-lang/clash-compiler/issues/251
     pipe 63. pipe 62. pipe 61. pipe 60.  pipe 59. pipe 58. pipe 57. pipe 56.
     pipe 55. pipe 54. pipe 53. pipe 52.  pipe 51. pipe 50. pipe 49. pipe 48.
     pipe 47. pipe 46. pipe 45. pipe 44.  pipe 43. pipe 42. pipe 41. pipe 40.
     pipe 39. pipe 38. pipe 37. pipe 36.  pipe 35. pipe 34. pipe 33. pipe 32.
     pipe 31. pipe 30. pipe 29. pipe 28.  pipe 27. pipe 26. pipe 25. pipe 24.
     pipe 23. pipe 22. pipe 21. pipe 20.  pipe 19. pipe 18. pipe 17. pipe 16.
     pipe 15. pipe 14. pipe 13. pipe 12.  pipe 11. pipe 10. pipe 9 . pipe 8 .
     pipe 7 . pipe 6 . pipe 5 . pipe 4 .  pipe 3 . pipe 2 . pipe 1 . pipe 0 .
     i0
   where
     pipe i (w, s) = step (pure $ kRom i) w s
     i0 m = (m, pure initial)
     finalize' = finalize . snd

{-# ANN naiveSingleBlockPipe'
  (defTop
    { t_name = "naiveSingleBlockPipe"
    }) #-}
-- | 'naiveSingleBlockPipe' specialized to the system clock.
naiveSingleBlockPipe' :: SystemClockReset
                      => Signal System Message
                      -> Signal System ShaState
naiveSingleBlockPipe' msg = toSignal $ naiveSingleBlockPipe $ fromSignal $ msg
{-# NOINLINE naiveSingleBlockPipe' #-}

-- | Turn a n bit 'BitVector', for n <= 447, into a single SHA-256 message
-- block by performing the SHA-256 preprocessing stage.
preprocessI :: (KnownNat n, n <= 447) => BitVector n -> Vec 16 (BitVector 32)
preprocessI m =
  let extended = resize (m ++# (1 :: BitVector 1)) :: BitVector 448
      shifted  = shiftL extended (447 - fromInteger (natVal m))
  in map v2bv (unconcat d32 $ bv2v $ shifted) ++ (0 :> fromInteger (natVal m) :> Nil)

-- | Turn a 'String' with length < 56 into a single SHA-256 message block by
-- performing the SHA-256 preprocessing stage.
preprocess :: String -> Vec 16 (BitVector 32)
preprocess m
  | P.length m > 55 = errorX "Can't turn strings with len > 55 into a single message block"
  | otherwise = map v2bv . unconcatI . bv2v $
                writeLen $
                flip shiftL (512-(8*(1+P.length m))) $
                (.|.) 0x80 . next $
                P.foldl (\bv c -> (next bv) .|. (cToBV c)) 0 m
    where
      cToBV c = (toEnum (C.ord c) :: BitVector 512)
      next bv = shiftL bv 8
      writeLen bv = bv .|. (8 * (toEnum $ P.length m))
