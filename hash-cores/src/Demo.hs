-- | Non-synthesizable demo functions
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Demo where

import           Clash.Prelude
import qualified HashCores.Sha256 as SHA256 (naiveSingleBlockPipe', preprocessI)
import           HashCores.Utils  (asciiLit)
import           Numeric          (showHex)

demo :: IO ()
demo = printX $ sampleN 129 top
  where
    top = withClockReset systemClockGen systemResetGen $
            fmap toHex $ SHA256.naiveSingleBlockPipe' $ pure $ msg

    msg = SHA256.preprocessI $ concatBitVector#
          $(asciiLit "The quick brown fox jumps over the lazy dog")
    -- msg = pure $ 0x80000000 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil

    toHex :: Vec 8 (BitVector 32) -> String
    toHex x = flip showHex "" $ concatBitVector# x
