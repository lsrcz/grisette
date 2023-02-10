{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Backend.SBV.Data.SMT.TermRewritingTests where

import Data.Foldable
import qualified Data.SBV as SBV
import Grisette.Backend.SBV.Data.SMT.Solving
import Grisette.Backend.SBV.Data.SMT.TermRewritingGen
import Grisette.Core.Data.Class.Solver
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Grisette.IR.SymPrim.Data.SymPrim
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

validateSpec :: (TermRewritingSpec a av, Show a, SupportedPrim av) => GrisetteSMTConfig n -> a -> Assertion
validateSpec config a = do
  r <- solve config (SymBool $ counterExample a)
  rs <- solve config (SymBool $ same a)
  case (r, rs) of
    (Left _, Right _) -> do
      return ()
    (Left _, Left _) -> do
      assertFailure $ "Bad rewriting with unsolvable formula: " ++ pformat (norewriteVer a) ++ " was rewritten to " ++ pformat (rewriteVer a)
    (Right m, _) -> do
      assertFailure $ "With model" ++ show m ++ "Bad rewriting: " ++ pformat (norewriteVer a) ++ " was rewritten to " ++ pformat (rewriteVer a)

termRewritingTests :: TestTree
termRewritingTests =
  let unboundedConfig = precise SBV.z3 -- {SBV.verbose=True}
   in testGroup
        "TermRewritingTests"
        [ testGroup
            "Bool only"
            [ testProperty "Bool only random test" $
                mapSize (`min` 10) $
                  ioProperty . \(x :: BoolOnlySpec) -> do
                    validateSpec unboundedConfig x,
              testCase "Regression nested ite with (ite a (ite b c d) e) with b is true" $ do
                validateSpec @BoolOnlySpec
                  unboundedConfig
                  ( iteSpec
                      (symSpec "a" :: BoolOnlySpec)
                      ( iteSpec
                          (orSpec (notSpec (andSpec (symSpec "b1") (symSpec "b2"))) (symSpec "b2") :: BoolOnlySpec)
                          (symSpec "c")
                          (symSpec "d")
                      )
                      (symSpec "e")
                  ),
              testCase "Regression for pevalImpliesTerm _ false should be false" $ do
                validateSpec @BoolOnlySpec
                  unboundedConfig
                  ( iteSpec
                      (symSpec "fbool" :: BoolOnlySpec)
                      ( notSpec
                          ( orSpec
                              (orSpec (notSpec (andSpec (symSpec "gbool" :: BoolOnlySpec) (symSpec "fbool" :: BoolOnlySpec))) (symSpec "gbool" :: BoolOnlySpec))
                              (orSpec (symSpec "abool" :: BoolOnlySpec) (notSpec (andSpec (symSpec "gbool" :: BoolOnlySpec) (symSpec "bbool" :: BoolOnlySpec))))
                          )
                      )
                      (symSpec "xxx" :: BoolOnlySpec)
                  )
            ],
          testGroup
            "LIA"
            [ testProperty "LIA random test" $
                mapSize (`min` 10) $
                  ioProperty . \(x :: LIAWithBoolSpec) -> do
                    validateSpec unboundedConfig x,
              testCase "Regression nested ite with (ite a b (ite c d e)) with c implies a" $ do
                validateSpec @LIAWithBoolSpec
                  unboundedConfig
                  ( iteSpec
                      (notSpec (eqvSpec (symSpec "v" :: LIAWithBoolSpec) (conSpec 1 :: LIAWithBoolSpec) :: BoolWithLIASpec))
                      (symSpec "b")
                      ( iteSpec
                          (eqvSpec (symSpec "v" :: LIAWithBoolSpec) (conSpec 2 :: LIAWithBoolSpec) :: BoolWithLIASpec)
                          (symSpec "d")
                          (symSpec "d")
                      )
                  )
            ],
          testGroup
            "Different sized SignedBV"
            [ testProperty "Fixed Sized SignedBV random test" $
                mapSize (`min` 10) $
                  ioProperty . \(x :: (DifferentSizeBVSpec IntN 4)) -> do
                    validateSpec unboundedConfig x
            ],
          testGroup
            "Fixed sized SignedBV"
            [ testProperty "Fixed Sized SignedBV random test" $
                mapSize (`min` 10) $
                  ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN)) -> do
                    validateSpec unboundedConfig x
            ],
          testGroup
            "timesNumSpec on integer"
            [ testCase "times on both concrete" $ do
                traverse_
                  (\(x, y) -> validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) (conSpec y))
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "times on single concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symSpec "a") (conSpec x)
                  )
                  [-3 .. 3],
              testCase "Two times with two concrete combined" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ timesNumSpec (conSpec y) (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ timesNumSpec (symSpec "a") (conSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symSpec "a") (conSpec x)) (conSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "Two times with one concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ timesNumSpec (symSpec "b") (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symSpec "b") $ timesNumSpec (symSpec "a") (conSpec x)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symSpec "b") $ timesNumSpec (conSpec x) (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (conSpec x) (symSpec "a")) (symSpec "b")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symSpec "a") (conSpec x)) (symSpec "b")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symSpec "a") (symSpec "b")) (conSpec x)
                  )
                  [-3 .. 3],
              testCase "times and add with two concretes combined" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ addNumSpec (conSpec y) (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ addNumSpec (symSpec "a") (conSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (addNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (addNumSpec (symSpec "a") (conSpec x)) (conSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ timesNumSpec (conSpec y) (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ timesNumSpec (symSpec "a") (conSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (timesNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (timesNumSpec (symSpec "a") (conSpec x)) (conSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "times concrete with uminusNumSpec symbolic" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) (uminusNumSpec $ symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (uminusNumSpec $ symSpec "a") (conSpec x)
                  )
                  [-3 .. 3]
            ],
          testGroup
            "DivI"
            [ testCase "DivI on concrete" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ divIntegerSpec (conSpec x) (conSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "DivI on single concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ divIntegerSpec (conSpec x) (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ divIntegerSpec (symSpec "a") (conSpec x)
                  )
                  [-3 .. 3]
            ],
          testGroup
            "ModI"
            [ testCase "ModI on concrete" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ modIntegerSpec (conSpec x) (conSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "ModI on single concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ modIntegerSpec (conSpec x) (symSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ modIntegerSpec (symSpec "a") (conSpec x)
                  )
                  [-3 .. 3]
            ]
        ]
