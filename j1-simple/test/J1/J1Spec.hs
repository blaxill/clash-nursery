{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module J1.J1Spec where


import           Clash.Prelude   as C
import qualified Prelude         as P

import           Test.Hspec
import           Test.QuickCheck

import           J1
import           J1.Ops

spec =
  describe "J1 Core" $ do
    it "writes to data stack when the stack increases" $
       property $ \a b c d e ->
        let a' = C.resize a
            b' = C.resize b
            c' = C.resize c
            d' = C.resize d
        in runInstructions dataStackWrite
          [Literal a, Literal b, Literal c, noop, Literal d, Literal e]
          == [Just 0, Just a', Just b', Nothing, Just c', Just d']
    it "writes to return stack when a call is made" $
       property $ \a b c d x y ->
        let x' = C.resize x
        in runInstructions returnStackWrite
          [Literal a, Literal b, Call x, noop, Literal c, Literal d, Call y]
          == [Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Just (x'*2 + 8)]
    it "sets ioTxEnable when N2IO is set" $
        runInstructions ioTxEnable
          [Literal 0, Literal 1, nextToIO, noop]
          == [False, False, True, False]
    it "sets tx address to top of stack and value to next of stack" $
       property $ \a b ->
        let a' = C.resize a
            b' = C.resize b
         in -- Drop the first element as it is an X value
          P.drop 1 (runInstructions tx
            [Literal a, Literal b, nextToIO, Literal 1, Literal 0, noop])
            == [ def{writeAddr=a', writeValue=0}
               , def{writeAddr=b', writeValue=a'}
               , def{writeAddr=b', writeValue=a'}
               , def{writeAddr=1, writeValue=b'}
               , def{writeAddr=0, writeValue=1}
               ]
