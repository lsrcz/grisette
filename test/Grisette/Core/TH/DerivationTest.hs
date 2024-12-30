{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques #-}
-- {-# OPTIONS_GHC -ddump-timings #-}

module Grisette.Core.TH.DerivationTest (derivationTest) where

import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Functor.Classes (showsPrec1, showsPrec2)
import qualified Data.Text as T
import GHC.TypeLits (KnownNat, type (<=))
import Grisette
  ( FP32,
    Mergeable,
    PPrint (pformat, pformatPrec),
    Solvable (con),
    SymEq ((.==)),
    SymOrd (symCompare),
    ValidFP,
    WordN32,
    docToTextWithWidth,
    mrgSingle,
    pformatPrec1,
    pformatPrec2,
    symCompare1,
    symCompare2,
    symEq1,
    symEq2,
  )
import Grisette.Core.TH.DerivationData
  ( Extra (Extra),
    GGG,
    Serializable,
    gggToVVV,
    replaceVVVShown,
  )
import Grisette.Core.TH.PartialEvalMode (PartialEvalMode)
import Grisette.Unified
  ( BaseMonad,
    EvalModeTag (C),
    GetBool,
    GetData,
    extractData,
  )
import qualified Grisette.Unified as GU
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck.Property ((.&.), (===))

#if MIN_VERSION_base(4,16,0)
ftst ::
  forall mode n eb sb a.
  ( PartialEvalMode mode,
    KnownNat n,
    1 <= n,
    ValidFP eb sb,
    Mergeable a
  ) =>
  GetBool mode ->
  GetData mode (Extra mode n eb sb a) ->
  GetData mode (Extra mode n eb sb a) ->
  BaseMonad mode (Extra mode n eb sb a)
ftst c t f =
  GU.mrgIf @mode
    c
    (extractData t)
    (extractData f)

derivationExtraTest :: [Test]
derivationExtraTest =
  [ testCase "ftst" $ do
      let x = Extra True [1 :: WordN32] [] (0 :: FP32) 0 0 (0 :: Int)
      let y = Extra False [1 :: WordN32] [] (0 :: FP32) 0 0 (0 :: Int)
      let a = ftst @'C True (return x) (return y)
      a @?= return x
  ]
#else
derivationExtraTest :: [Test]
derivationExtraTest = []
#endif

derivationTest :: Test
derivationTest =
  testGroup "Derivation" $
    [ testProperty "GADT Show instance for regular types" $
        \(g :: GGG (GGG Int String) [Int]) ->
          let v = gggToVVV g
           in replaceVVVShown (T.pack (show g))
                === replaceVVVShown (T.pack (show v)),
      testProperty "GADT Show and Show1 are consistent" $
        \(g :: GGG (GGG Char String) [Int]) ->
          T.pack (show g)
            === T.pack (showsPrec1 0 g "")
            .&. T.pack (showsPrec 11 g "")
            === T.pack (showsPrec1 11 g ""),
      testProperty "GADT Show and Show2 are consistent" $
        \(g :: GGG (GGG Char String) [Int]) ->
          T.pack (show g)
            === T.pack (showsPrec2 0 g "")
            .&. T.pack (showsPrec 11 g "")
            === T.pack (showsPrec2 11 g ""),
      testProperty "GADT PPrint instance for regular types" $
        \(g :: GGG (GGG Int String) [Int]) ->
          let v = gggToVVV g
           in replaceVVVShown (docToTextWithWidth 1000 (pformat g))
                === replaceVVVShown (docToTextWithWidth 1000 (pformat v))
                .&. replaceVVVShown (docToTextWithWidth 0 (pformat g))
                === replaceVVVShown (docToTextWithWidth 0 (pformat v)),
      testProperty "GADT PPrint and PPrint1 are consistent" $
        \(g :: GGG (GGG Char String) [Int]) ->
          docToTextWithWidth 1000 (pformatPrec 0 g)
            === docToTextWithWidth 1000 (pformatPrec1 0 g)
            .&. docToTextWithWidth 1000 (pformatPrec 11 g)
            === docToTextWithWidth 1000 (pformatPrec1 11 g)
            .&. docToTextWithWidth 0 (pformatPrec 0 g)
            === docToTextWithWidth 0 (pformatPrec1 0 g)
            .&. docToTextWithWidth 0 (pformatPrec 11 g)
            === docToTextWithWidth 0 (pformatPrec1 11 g),
      testProperty "GADT PPrint and PPrint2 are consistent" $
        \(g :: GGG (GGG Char String) [Int]) ->
          docToTextWithWidth 1000 (pformatPrec 0 g)
            === docToTextWithWidth 1000 (pformatPrec2 0 g)
            .&. docToTextWithWidth 1000 (pformatPrec 11 g)
            === docToTextWithWidth 1000 (pformatPrec2 11 g)
            .&. docToTextWithWidth 0 (pformatPrec 0 g)
            === docToTextWithWidth 0 (pformatPrec2 0 g)
            .&. docToTextWithWidth 0 (pformatPrec 11 g)
            === docToTextWithWidth 0 (pformatPrec2 11 g),
      testProperty "GADT SymEq and Eq are consistent" $
        \(g1 :: GGG (GGG Int String) [Int]) g2 ->
          let v1 = gggToVVV g1
              v2 = gggToVVV g2
           in con (g1 == g2) === (v1 .== v2),
      testProperty "GADT SymEq1 and SymEq are consistent" $
        \(g1 :: GGG (GGG Int String) [Int]) g2 ->
          symEq1 g1 g2 === (g1 .== g2),
      testProperty "GADT SymEq2 and SymEq are consistent" $
        \(g1 :: GGG (GGG Int String) [Int]) g2 ->
          symEq2 g1 g2 === (g1 .== g2),
      testProperty "GADT SymOrd and Ord are consistent" $
        \(g1 :: GGG (GGG Int String) [Int]) g2 ->
          let v1 = gggToVVV g1
              v2 = gggToVVV g2
           in mrgSingle (g1 `compare` g2) === (v1 `symCompare` v2),
      testProperty "GADT SymOrd1 and SymOrd are consistent" $
        \(g1 :: GGG (GGG Int String) [Int]) g2 ->
          symCompare1 g1 g2 === (g1 `symCompare` g2),
      testProperty "GADT SymOrd2 and SymOrd are consistent" $
        \(g1 :: GGG (GGG Int String) [Int]) g2 ->
          symCompare2 g1 g2 === (g1 `symCompare` g2),
      testProperty "Serialize" $ do
        \(s :: Serializable Int) ->
          let bs = runPutS (serialize s)
              s' = runGetS deserialize bs
           in Right s === s'
    ]
      ++ derivationExtraTest
