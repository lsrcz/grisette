{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Grisette.SymPrim.Prim.ConcurrentTests (concurrentTests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Hashable (Hashable (hash))
import Data.String (IsString (fromString))
import Grisette (SymInteger (SymInteger))
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
        hash ar @?= hash br
    ]
