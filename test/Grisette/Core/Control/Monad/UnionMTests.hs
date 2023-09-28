{-# LANGUAGE OverloadedStrings #-}

module Grisette.Core.Control.Monad.UnionMTests (unionMTests) where

import Grisette.Core.Control.Monad.UnionM (UnionM, unionSize)
import Grisette.Core.Data.Class.GenSym (choose)
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionLike (single),
    mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (ssym))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

unionMTests :: TestTree
unionMTests =
  testGroup
    "UnionMTests"
    [ testCase "unionSize" $ do
        unionSize (single 1 :: UnionM Integer) @=? 1
        unionSize (mrgIf (ssym "a") (single 1) (single 2) :: UnionM Integer) @=? 2
        unionSize (choose [1, 2, 3, 4, 5, 6, 7] "a" :: UnionM Integer) @=? 7
    ]
