{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Pizza.TestUtils.SOrd where

import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.SOrd
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Hspec

concreteOrdOkProp :: (HasCallStack, SOrd SBool a, Ord a) => (a, a) -> Expectation
concreteOrdOkProp (i, j) = do
  i <=~ j `shouldBe` CBool (i <= j)
  i <~ j `shouldBe` CBool (i < j)
  i >=~ j `shouldBe` CBool (i >= j)
  i >~ j `shouldBe` CBool (i > j)
  symCompare i j `shouldBe` (mrgReturn $ compare i j :: UnionMBase SBool Ordering)

symbolicProdOrdOkProp ::
  (HasCallStack, Show v, Show vl, Show vr, SOrd SBool v, SOrd SBool vl, SOrd SBool vr) =>
  v ->
  v ->
  vl ->
  vr ->
  vl ->
  vr ->
  Expectation
symbolicProdOrdOkProp l r ll lr rl rr = do
  l <=~ r `shouldBe` ((ll <~ rl) ||~ ((ll ==~ rl) &&~ (lr <=~ rr)) :: SBool)
  l <~ r `shouldBe` ((ll <~ rl) ||~ ((ll ==~ rl) &&~ (lr <~ rr)) :: SBool)
  l >=~ r `shouldBe` ((ll >~ rl) ||~ ((ll ==~ rl) &&~ (lr >=~ rr)) :: SBool)
  l >~ r `shouldBe` ((ll >~ rl) ||~ ((ll ==~ rl) &&~ (lr >~ rr)) :: SBool)
  l
    `symCompare` r
    `shouldBe` ( ( do
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
  Expectation
symbolicSumOrdOkProp li ri lli lri rli rri = go li ri lli lri rli rri (0 :: Int) (0 :: Int)
  where
    go [] _ [] [] _ _ _ _ = return ()
    go (lv : ls) r (llv : lls) (lrv : lrs) rl rr ln rn = do
      gor lv r llv lrv rl rr ln rn
      go ls r lls lrs rl rr (ln + 1) rn
    go _ _ _ _ _ _ _ _ = False `shouldBe` True
    gor _ [] _ _ [] [] _ _ = return ()
    gor lv (rv : rs) llv lrv (rlv : rls) (rrv : rrs) ln rn
      | ln < rn = do
          lv <=~ rv `shouldBe` CBool True
          lv <~ rv `shouldBe` CBool True
          lv >=~ rv `shouldBe` CBool False
          lv >~ rv `shouldBe` CBool False
          lv `symCompare` rv `shouldBe` (mrgReturn LT :: UnionMBase SBool Ordering)
          gor lv rs llv lrv rls rrs ln (rn + 1)
      | ln == rn = do
          symbolicProdOrdOkProp lv rv llv lrv rlv rrv
          gor lv rs llv lrv rls rrs ln (rn + 1)
      | otherwise = do
          lv <=~ rv `shouldBe` CBool False
          lv <~ rv `shouldBe` CBool False
          lv >=~ rv `shouldBe` CBool True
          lv >~ rv `shouldBe` CBool True
          lv `symCompare` rv `shouldBe` (mrgReturn GT :: UnionMBase SBool Ordering)
          gor lv rs llv lrv rls rrs ln (rn + 1)
    gor _ _ _ _ _ _ _ _ = False `shouldBe` True
