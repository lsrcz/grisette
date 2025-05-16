{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.SymPrim.QuantifierTests (quantifierTests) where

import Grisette
  ( AsKey (AsKey),
    Function ((#)),
    GenSymSimple (simpleFresh),
    LogicalOp (symImplies),
    ModelOps (isEmptyModel),
    SimpleListSpec (SimpleListSpec),
    SymEq ((.==)),
    SymOrd ((.>)),
    runFresh,
    solve,
    z3,
  )
import Grisette.Internal.SymPrim.Quantifier (existsFresh, existsSym, forallFresh, forallSym)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.SymPrim (type (=~>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

#if MIN_VERSION_sbv(10,1,0)
sbvVersionCheck :: Test -> Test
sbvVersionCheck = id
#else
sbvVersionCheck :: Test -> Test
sbvVersionCheck _ = testGroup "Quantifier" []
#endif

quantifierTests :: Test
quantifierTests =
  sbvVersionCheck $
    testGroup
      "Quantifier"
      [ testCase "Basic" $ do
          let l@[x, y] = ["x", "y"] :: [SymInteger]
          r <- solve z3 $ forallSym l $ (x .== y) `symImplies` (x + 1 .== y + 1)
          case r of
            Left err -> error $ show err
            Right mo ->
              assertBool "no elements should be in the model" $ isEmptyModel mo,
        testCase "Basic2" $ do
          let [x, y] = ["x", "y"] :: [SymInteger]
          r <- solve z3 $ forallSym x $ existsSym y $ x .> y
          case r of
            Left err -> error $ show err
            Right mo ->
              assertBool "no elements should be in the model" $ isEmptyModel mo,
        testCase "With ufunc" $ do
          -- https://github.com/LeventErkok/sbv/issues/711
          let f = "f" :: SymInteger =~> SymInteger
          let x = "x" :: SymInteger
          let y = "y" :: SymInteger
          r <-
            solve z3 $
              forallSym x $
                forallSym y $
                  (x .== y) `symImplies` ((f # x) .== (f # y))
          case r of
            Left err -> error $ show err
            Right _ -> return (),
        testCase "fresh" $ do
          let x = flip runFresh "x" $
                forallFresh (SimpleListSpec 2 ()) $ \(l :: [SymInteger]) ->
                  existsFresh (SimpleListSpec 2 ()) $ \(r :: [SymInteger]) ->
                    return $ l .== r
          let r = flip runFresh "x" $ do
                l :: [SymInteger] <- simpleFresh (SimpleListSpec 2 ())
                r <- simpleFresh (SimpleListSpec 2 ())
                return $ forallSym l $ existsSym r $ l .== r
          AsKey x @?= AsKey r
      ]
