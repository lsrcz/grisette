{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Grisette.SymPrim.Prim.ConcurrentTests (concurrentTests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Hashable (Hashable (hash))
import Data.String (IsString (fromString))
import Grisette (SymEq ((.==)), SymInteger (SymInteger), evalSymToCon, solve, z3)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

concurrentTests :: Test
concurrentTests =
  testGroup
    "Concurrent"
    [ testCase "Consistent hash/eq" $ do
        aref <- newEmptyMVar
        bref <- newEmptyMVar
        _ <- forkIO $ do
          evaluate $ force (map (fromString . show) [1 .. 1000] :: [SymInteger])
          evaluate $ force ("x" :: SymInteger)
          SymInteger p <- evaluate $ force ("y" + "z" :: SymInteger)
          putMVar aref p
        ar <- takeMVar aref
        _ <- forkIO $ do
          SymInteger p <- evaluate $ force ("y" + "z" :: SymInteger)
          putMVar bref p
        br <- takeMVar bref
        ar @?= br
        hash ar @?= hash br,
      testCase "Eval" $ do
        aref <- newEmptyMVar
        bref <- newEmptyMVar
        _ <- forkIO $ do
          a <- evaluate $ force ("a" :: SymInteger)
          putMVar aref a
        _ <- forkIO $ do
          b <- evaluate $ force ("b" :: SymInteger)
          putMVar bref b
        a <- takeMVar aref
        b <- takeMVar bref
        r <- solve z3 $ a .== b
        case r of
          Left err -> error $ show err
          Right m -> evalSymToCon m a @?= (evalSymToCon m b :: Integer)
    ]
