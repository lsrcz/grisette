{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Pizza.TestUtils.SOrd where

import GHC.Stack
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.SOrd
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.Assertions
import Pizza.TestUtils.SBool

concreteOrdOkProp :: (HasCallStack, SOrd SBool a, Ord a) => (a, a) -> Assertion
concreteOrdOkProp (i, j) = do
  i <=~ j @=? CBool (i <= j)
  i <~ j @=? CBool (i < j)
  i >=~ j @=? CBool (i >= j)
  i >~ j @=? CBool (i > j)
  symCompare i j @=? (mrgReturn $ compare i j :: UnionMBase SBool Ordering)

symbolicProdOrdOkProp ::
  (HasCallStack, Show v, Show vl, Show vr, SOrd SBool v, SOrd SBool vl, SOrd SBool vr) =>
  v ->
  v ->
  vl ->
  vr ->
  vl ->
  vr ->
  Assertion
symbolicProdOrdOkProp l r ll lr rl rr = do
  l <=~ r @=? ((ll <~ rl) ||~ ((ll ==~ rl) &&~ (lr <=~ rr)) :: SBool)
  l <~ r @=? ((ll <~ rl) ||~ ((ll ==~ rl) &&~ (lr <~ rr)) :: SBool)
  l >=~ r @=? ((ll >~ rl) ||~ ((ll ==~ rl) &&~ (lr >=~ rr)) :: SBool)
  l >~ r @=? ((ll >~ rl) ||~ ((ll ==~ rl) &&~ (lr >~ rr)) :: SBool)
  l
    `symCompare` r
    @=? ( ( do
              lc <- symCompare ll rl
              case lc of
                EQ -> symCompare lr rr
                _ -> mrgReturn lc
          ) ::
            UnionMBase SBool Ordering
        )

symbolicSumOrdOkProp ::
  forall v vl vr.
  (HasCallStack, Show v, Show vl, Show vr, SOrd SBool v, SOrd SBool vl, SOrd SBool vr) =>
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
          lv <=~ rv @=? CBool True
          lv <~ rv @=? CBool True
          lv >=~ rv @=? CBool False
          lv >~ rv @=? CBool False
          lv `symCompare` rv @=? (mrgReturn LT :: UnionMBase SBool Ordering)
          gor lv rs llv lrv rls rrs ln (rn + 1)
      | ln == rn = do
          symbolicProdOrdOkProp lv rv llv lrv rlv rrv
          gor lv rs llv lrv rls rrs ln (rn + 1)
      | otherwise = do
          lv <=~ rv @=? CBool False
          lv <~ rv @=? CBool False
          lv >=~ rv @=? CBool True
          lv >~ rv @=? CBool True
          lv `symCompare` rv @=? (mrgReturn GT :: UnionMBase SBool Ordering)
          gor lv rs llv lrv rls rrs ln (rn + 1)
    gor _ _ _ _ _ _ _ _ = False @=? True
