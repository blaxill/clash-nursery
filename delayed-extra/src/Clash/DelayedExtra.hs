-- | Extra functions for working with DSignal types

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Clash.DelayedExtra (
  delayD,
  delayDI,
  pipelineFold,
  BundleD,
  UnbundledD,
  bundleD,
  unbundleD,
  ) where

import           Clash.Prelude
import           Data.Proxy
import           Data.Singletons.Prelude

import qualified Prelude                 as P

-- | 'DSignal' variant of 'delay'
delayD :: (HasClock domain gated)
       => DSignal domain d     a
       -> DSignal domain (d+1) a
delayD = unsafeFromSignal . delay . toSignal

-- | 'DSignal' variant of 'delayI'
delayDI :: forall domain gated d k a. (HasClock domain gated, KnownNat k)
        => DSignal domain d     a
        -> DSignal domain (d+k) a
delayDI = unsafeFromSignal . go (snatToNum (SNat :: SNat k)) . toSignal
  where
    go 0 = id
    go n = delay . go (n-1)

data IDSignal (domain :: Domain) (d :: Nat) (a :: *) (f :: TyFun Nat *) :: *
type instance Apply (IDSignal domain d a) k = DSignal domain (d+k) a

-- | A tree structured fold that inserts a flip-flop after every operation.
-- Uses 'dtfold' internally for delay calculation.
pipelineFold :: forall domain gated synchronous d k a.
              ( HasClockReset domain gated synchronous
              , Default a
              , KnownNat d
              , KnownNat k
              , KnownNat (2^k))
             => (a -> a -> a)                  -- ^ Fold operation to apply
             -> Vec (2^k) (DSignal domain d a) -- ^ 2^k vector input
             -> DSignal domain (d + k) a       -- ^ Output delayed by k
pipelineFold op = dtfold (Proxy :: Proxy (IDSignal domain d a)) id go
  where
    go :: SNat l
       -> DSignal domain (d + l) a
       -> IDSignal domain d a @@ l
       -> IDSignal domain d a @@ (l+1)
    go SNat x y = delayD (op <$> x <*> y)

-- | A 'DSignal' variant of 'Bundle'
class BundleD a where
  type UnbundledD (domain :: Domain) (d :: Nat) a = res | res -> domain d
  type UnbundledD domain d a = DSignal domain d a

  bundleD :: UnbundledD domain d a -> DSignal domain d a
  {-# INLINE bundleD #-}
  default bundleD :: (DSignal domain d a ~ UnbundledD domain d a)
                 => UnbundledD domain d a -> DSignal domain d a
  bundleD s = s

  unbundleD :: DSignal domain d a -> UnbundledD domain d a
  {-# INLINE unbundleD #-}
  default unbundleD :: (UnbundledD domain d a ~ DSignal domain d a)
                   => DSignal domain d a -> UnbundledD domain d a
  unbundleD s = s

instance BundleD Bool
instance BundleD Integer
instance BundleD Int
instance BundleD Float
instance BundleD Double
instance BundleD (Maybe a)
instance BundleD (Either a b)

instance BundleD (BitVector n)
instance BundleD (Index n)
instance BundleD (Fixed rep int frac)
instance BundleD (Signed n)
instance BundleD (Unsigned n)

instance BundleD (a,b) where
  type UnbundledD t d (a,b) = (DSignal t d a, DSignal t d b)
  bundleD       = uncurry (liftA2 (,))
  unbundleD tup = (fmap fst tup, fmap snd tup)

instance BundleD (a,b,c) where
  type UnbundledD t d (a,b,c) = (DSignal t d a, DSignal t d b, DSignal t d c)
  bundleD   (a,b,c) = (,,) <$> a <*> b <*> c
  unbundleD tup     = (fmap (\(x,_,_) -> x) tup
                      ,fmap (\(_,x,_) -> x) tup
                      ,fmap (\(_,_,x) -> x) tup
                      )
instance BundleD (a,b,c,d) where
  type UnbundledD t delay (a,b,c,d) =
    (DSignal t delay a, DSignal t delay b, DSignal t delay c, DSignal t delay d)
  bundleD   (a,b,c,d) = (,,,) <$> a <*> b <*> c <*> d
  unbundleD tup     = (fmap (\(x,_,_,_) -> x) tup
                      ,fmap (\(_,x,_,_) -> x) tup
                      ,fmap (\(_,_,x,_) -> x) tup
                      ,fmap (\(_,_,_,x) -> x) tup
                      )

instance KnownNat n => BundleD (Vec n a) where
  type UnbundledD t d (Vec n a) = Vec n (DSignal t d a)
  bundleD   = vecBundleD#
  unbundleD = sequenceA . fmap lazyV

{-# NOINLINE vecBundleD# #-}
vecBundleD# :: Vec n (DSignal t d a) -> DSignal t d (Vec n a)
vecBundleD# = traverse# id
