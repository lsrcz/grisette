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
  r <- solve config (Sym $ counterExample a)
  rs <- solve config (Sym $ same a)
  case (r, rs) of
    (Left _, Right _) -> do
      return ()
    (Left _, Left _) -> do
      assertFailure $ "Bad rewriting with unsolvable formula: " ++ pformat (norewriteVer a) ++ " was rewritten to " ++ pformat (rewriteVer a)
    (Right m, _) -> do
      assertFailure $ "With model" ++ show m ++ "Bad rewriting: " ++ pformat (norewriteVer a) ++ " was rewritten to " ++ pformat (rewriteVer a)

termRewritingTests :: TestTree
termRewritingTests =
  let unboundedConfig = UnboundedReasoning SBV.z3 -- {SBV.verbose=True}
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
                      (symbSpec "a" :: BoolOnlySpec)
                      ( iteSpec
                          (orSpec (notSpec (andSpec (symbSpec "b1") (symbSpec "b2"))) (symbSpec "b2") :: BoolOnlySpec)
                          (symbSpec "c")
                          (symbSpec "d")
                      )
                      (symbSpec "e")
                  ),
              testCase "Regression for pevalImpliesTerm _ false should be false" $ do
                validateSpec @BoolOnlySpec
                  unboundedConfig
                  ( iteSpec
                      (symbSpec "fbool" :: BoolOnlySpec)
                      ( notSpec
                          ( orSpec
                              (orSpec (notSpec (andSpec (symbSpec "gbool" :: BoolOnlySpec) (symbSpec "fbool" :: BoolOnlySpec))) (symbSpec "gbool" :: BoolOnlySpec))
                              (orSpec (symbSpec "abool" :: BoolOnlySpec) (notSpec (andSpec (symbSpec "gbool" :: BoolOnlySpec) (symbSpec "bbool" :: BoolOnlySpec))))
                          )
                      )
                      (symbSpec "xxx" :: BoolOnlySpec)
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
                      (notSpec (eqvSpec (symbSpec "v" :: LIAWithBoolSpec) (concSpec 1 :: LIAWithBoolSpec) :: BoolWithLIASpec))
                      (symbSpec "b")
                      ( iteSpec
                          (eqvSpec (symbSpec "v" :: LIAWithBoolSpec) (concSpec 2 :: LIAWithBoolSpec) :: BoolWithLIASpec)
                          (symbSpec "d")
                          (symbSpec "d")
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
                  (\(x, y) -> validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) (concSpec y))
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "times on single concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symbSpec "a") (concSpec x)
                  )
                  [-3 .. 3],
              testCase "Two times with two concrete combined" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) $ timesNumSpec (concSpec y) (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) $ timesNumSpec (symbSpec "a") (concSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (concSpec x) (symbSpec "a")) (concSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symbSpec "a") (concSpec x)) (concSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "Two times with one concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) $ timesNumSpec (symbSpec "b") (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symbSpec "b") $ timesNumSpec (symbSpec "a") (concSpec x)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symbSpec "b") $ timesNumSpec (concSpec x) (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (concSpec x) (symbSpec "a")) (symbSpec "b")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symbSpec "a") (concSpec x)) (symbSpec "b")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symbSpec "a") (symbSpec "b")) (concSpec x)
                  )
                  [-3 .. 3],
              testCase "times and add with two concretes combined" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) $ addNumSpec (concSpec y) (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) $ addNumSpec (symbSpec "a") (concSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (addNumSpec (concSpec x) (symbSpec "a")) (concSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (addNumSpec (symbSpec "a") (concSpec x)) (concSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (concSpec x) $ timesNumSpec (concSpec y) (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (concSpec x) $ timesNumSpec (symbSpec "a") (concSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (timesNumSpec (concSpec x) (symbSpec "a")) (concSpec y)
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (timesNumSpec (symbSpec "a") (concSpec x)) (concSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "times concrete with uminusNumSpec symbolic" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (concSpec x) (uminusNumSpec $ symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (uminusNumSpec $ symbSpec "a") (concSpec x)
                  )
                  [-3 .. 3]
            ],
          testGroup
            "DivI"
            [ testCase "DivI on concrete" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ divIntegerSpec (concSpec x) (concSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "DivI on single concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ divIntegerSpec (concSpec x) (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ divIntegerSpec (symbSpec "a") (concSpec x)
                  )
                  [-3 .. 3]
            ],
          testGroup
            "ModI"
            [ testCase "ModI on concrete" $ do
                traverse_
                  ( \(x, y) -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ modIntegerSpec (concSpec x) (concSpec y)
                  )
                  [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
              testCase "ModI on single concrete" $ do
                traverse_
                  ( \x -> do
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ modIntegerSpec (concSpec x) (symbSpec "a")
                      validateSpec @(GeneralSpec Integer) unboundedConfig $ modIntegerSpec (symbSpec "a") (concSpec x)
                  )
                  [-3 .. 3]
            ]
        ]
