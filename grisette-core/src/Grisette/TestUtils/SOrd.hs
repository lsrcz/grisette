{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.TestUtils.SOrd
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.TestUtils.SOrd where

import GHC.Stack
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.SOrd
import Grisette.Lib.Control.Monad
import Grisette.TestUtils.Assertions
import Grisette.TestUtils.SBool

concreteOrdOkProp :: (HasCallStack, GSOrd SBool a, Ord a) => (a, a) -> Assertion
concreteOrdOkProp (i, j) = do
  i `gsymle` j @=? CBool (i <= j)
  i `gsymlt` j @=? CBool (i < j)
  i `gsymge` j @=? CBool (i >= j)
  i `gsymgt` j @=? CBool (i > j)
  gsymCompare i j @=? (mrgReturn $ compare i j :: UnionMBase SBool Ordering)

symbolicProdOrdOkProp ::
  (HasCallStack, Show v, Show vl, Show vr, GSOrd SBool v, GSOrd SBool vl, GSOrd SBool vr) =>
  v ->
  v ->
  vl ->
  vr ->
  vl ->
  vr ->
  Assertion
symbolicProdOrdOkProp l r ll lr rl rr = do
  l `gsymle` r @=? ((ll `gsymlt` rl) ||~ ((ll `gsymeq` rl) &&~ (lr `gsymle` rr)) :: SBool)
  l `gsymlt` r @=? ((ll `gsymlt` rl) ||~ ((ll `gsymeq` rl) &&~ (lr `gsymlt` rr)) :: SBool)
  l `gsymge` r @=? ((ll `gsymgt` rl) ||~ ((ll `gsymeq` rl) &&~ (lr `gsymge` rr)) :: SBool)
  l `gsymgt` r @=? ((ll `gsymgt` rl) ||~ ((ll `gsymeq` rl) &&~ (lr `gsymgt` rr)) :: SBool)
  l
    `gsymCompare` r
    @=? ( ( do
              lc <- gsymCompare ll rl
              case lc of
                EQ -> gsymCompare lr rr
                _ -> mrgReturn lc
          ) ::
            UnionMBase SBool Ordering
        )

symbolicSumOrdOkProp ::
  forall v vl vr.
  (HasCallStack, Show v, Show vl, Show vr, GSOrd SBool v, GSOrd SBool vl, GSOrd SBool vr) =>
  [v] ->
  [v] ->
  [vl] ->
  [vr] ->
  [vl] ->
  [vr] ->
  Assertion
symbolicSumOrdOkProp li ri lli lri rli rri = go li ri lli lri rli rri (0 :: Int) (0 :: Int)
  where
    go [] _ [] [] _ _ _ _ = return ()
    go (lv : ls) r (llv : lls) (lrv : lrs) rl rr ln rn = do
      gor lv r llv lrv rl rr ln rn
      go ls r lls lrs rl rr (ln + 1) rn
    go _ _ _ _ _ _ _ _ = False @=? True
    gor _ [] _ _ [] [] _ _ = return ()
    gor lv (rv : rs) llv lrv (rlv : rls) (rrv : rrs) ln rn
      | ln < rn = do
          lv `gsymle` rv @=? CBool True
          lv `gsymlt` rv @=? CBool True
          lv `gsymge` rv @=? CBool False
          lv `gsymgt` rv @=? CBool False
          lv `gsymCompare` rv @=? (mrgReturn LT :: UnionMBase SBool Ordering)
          gor lv rs llv lrv rls rrs ln (rn + 1)
      | ln == rn = do
          symbolicProdOrdOkProp lv rv llv lrv rlv rrv
          gor lv rs llv lrv rls rrs ln (rn + 1)
      | otherwise = do
          lv `gsymle` rv @=? CBool False
          lv `gsymlt` rv @=? CBool False
          lv `gsymge` rv @=? CBool True
          lv `gsymgt` rv @=? CBool True
          lv `gsymCompare` rv @=? (mrgReturn GT :: UnionMBase SBool Ordering)
          gor lv rs llv lrv rls rrs ln (rn + 1)
    gor _ _ _ _ _ _ _ _ = False @=? True
