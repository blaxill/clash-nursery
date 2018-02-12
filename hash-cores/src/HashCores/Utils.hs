-- | Extra utility functions

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module HashCores.Utils where

import           Clash.Prelude
import qualified Prelude             as P

import           Data.Char           (ord)
import           Language.Haskell.TH (Exp, ExpQ, Q, TExp, TypeQ)

-- | ASCII literal to 'Vec' of 8 bit 'BitVector's.
asciiLit :: String -> ExpQ
asciiLit []     = [| Nil |]
asciiLit (x:xs) = [| (toEnum (ord x) :: BitVector 8) :> $(asciiLit xs) |]
