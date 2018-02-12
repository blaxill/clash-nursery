{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}

module HashCores.Sha256Spec where

import           Clash.Prelude
import qualified Prelude               as P

import           Test.Hspec
import           Test.QuickCheck       hiding ((.&.))

import           HashCores.Sha256

import           Crypto.Hash.SHA256    as H
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8

newtype SingleMessageBlock = SingleMessageBlock B.ByteString
  deriving (Eq, Ord, Show, Read)

shrinks :: B.ByteString -> [B.ByteString]
shrinks bs =
  [ B.append a b | (a, b) <- P.zip (B.inits bs) (P.tail $ B.tails bs) ]

instance Arbitrary SingleMessageBlock where
  arbitrary = do
    k <- choose (0, 55)
    (SingleMessageBlock . B.pack) <$> vectorOf k (choose (0, 255))
  shrink (SingleMessageBlock b) = SingleMessageBlock <$> shrinks b

hashCoreSha256 :: SingleMessageBlock -> Integer
hashCoreSha256 (SingleMessageBlock s) =
    foldl (\acc c -> (shiftL acc 32) + toInteger c) 0 $
    P.last . sampleN 129 $
    withClockReset systemClockGen systemResetGen $
    naiveSingleBlockPipe' $
    pure $ preprocess (B8.unpack s)

nativeSha256 :: SingleMessageBlock -> Integer
nativeSha256 (SingleMessageBlock s) =
  B.foldl (\acc c -> (shiftL acc 8) + toInteger c) 0 $
  H.hash s

spec = do
  describe "SHA-256 (naiveSingleBlockPipe)" $ do
      it "hash of empty string matches specification" $ do
        hashCoreSha256 (SingleMessageBlock $ B.pack []) ==
          0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
      it "hash of 'The quick brown fox jumps over the lazy dog' matches specification" $ do
        hashCoreSha256 (SingleMessageBlock $ B8.pack
          "The quick brown fox jumps over the lazy dog") ==
          0xd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592
      it "hashes a single message-block to SHA-256" $ do
        property $ \x -> hashCoreSha256 x == nativeSha256 x
