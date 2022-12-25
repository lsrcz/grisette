{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.TestUtils.SBool
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.TestUtils.SBool where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.String
import Data.Typeable
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.Lib.Control.Monad

data SBool where
  CBool :: Bool -> SBool
  SSBool :: String -> SBool
  ISSBool :: (Typeable a, Show a, Eq a, Hashable a) => String -> a -> SBool
  ISBool :: String -> Int -> SBool
  IISBool :: (Typeable a, Show a, Eq a, Hashable a) => String -> Int -> a -> SBool
  Or :: SBool -> SBool -> SBool
  And :: SBool -> SBool -> SBool
  Not :: SBool -> SBool
  Equal :: SBool -> SBool -> SBool
  ITE :: SBool -> SBool -> SBool -> SBool

instance Show SBool where
  show (CBool v) = "CBool " ++ show v
  show (SSBool s) = "SSBool " ++ s
  show (ISSBool s info) = "ISSBool " ++ s ++ " " ++ show info
  show (ISBool s i) = "ISBool " ++ show i ++ " " ++ s
  show (IISBool s i info) = "IISBool " ++ show i ++ " " ++ show s ++ " " ++ show info
  show (Or l r) = "Or (" ++ show l ++ ") (" ++ show r ++ ")"
  show (And l r) = "And (" ++ show l ++ ") (" ++ show r ++ ")"
  show (Not v) = "Not (" ++ show v ++ ")"
  show (Equal l r) = "Equal (" ++ show l ++ ") (" ++ show r ++ ")"
  show (ITE c l r) = "ITE (" ++ show c ++ ") (" ++ show l ++ ") (" ++ show r ++ ")"

instance Eq SBool where
  CBool l == CBool r = l == r
  SSBool l == SSBool r = l == r
  ISSBool s1 (info1 :: a) == ISSBool s2 (info2 :: b) = case eqT @a @b of
    Just Refl -> s1 == s2 && info1 == info2
    Nothing -> False
  ISBool i1 s1 == ISBool i2 s2 = i1 == i2 && s1 == s2
  IISBool i1 s1 (info1 :: a) == IISBool i2 s2 (info2 :: b) = case eqT @a @b of
    Just Refl -> i1 == i2 && s1 == s2 && info1 == info2
    Nothing -> False
  Or l1 r1 == Or l2 r2 = l1 == l2 && r1 == r2
  And l1 r1 == And l2 r2 = l1 == l2 && r1 == r2
  Not v1 == Not v2 = v1 == v2
  Equal l1 r1 == Equal l2 r2 = l1 == l2 && r1 == r2
  ITE c1 l1 r1 == ITE c2 l2 r2 = c1 == c2 && l1 == l2 && r1 == r2
  _ == _ = False

instance GMergeable SBool SBool where
  grootStrategy = SimpleStrategy ites

instance GSimpleMergeable SBool SBool where
  gmrgIte = ites

instance GEvaluateSym (M.HashMap Symbol Bool) SBool where
  gevaluateSym _ _ c@(CBool _) = c
  gevaluateSym fillDefault model s@(SSBool sym) = case M.lookup (SSymbol sym) model of
    Just v -> CBool v
    Nothing -> if fillDefault then CBool False else s
  gevaluateSym fillDefault model s@(ISSBool sym info) = case M.lookup (ISSymbol sym info) model of
    Just v -> CBool v
    Nothing -> if fillDefault then CBool False else s
  gevaluateSym fillDefault model s@(ISBool sym i) = case M.lookup (ISymbol sym i) model of
    Just v -> CBool v
    Nothing -> if fillDefault then CBool False else s
  gevaluateSym fillDefault model s@(IISBool sym i info) = case M.lookup (IISymbol sym i info) model of
    Just v -> CBool v
    Nothing -> if fillDefault then CBool False else s
  gevaluateSym fillDefault model (Or l r) = gevaluateSym fillDefault model l ||~ gevaluateSym fillDefault model r
  gevaluateSym fillDefault model (And l r) = gevaluateSym fillDefault model l &&~ gevaluateSym fillDefault model r
  gevaluateSym fillDefault model (Not v) = nots (gevaluateSym fillDefault model v)
  gevaluateSym fillDefault model (Equal l r) = gevaluateSym fillDefault model l `gsymeq` gevaluateSym fillDefault model r
  gevaluateSym fillDefault model (ITE c l r) =
    ites
      (gevaluateSym fillDefault model c)
      (gevaluateSym fillDefault model l)
      (gevaluateSym fillDefault model r)

instance GSEq SBool SBool where
  (CBool l) `gsymeq` (CBool r) = CBool (l == r)
  (CBool True) `gsymeq` r = r
  (CBool False) `gsymeq` r = nots r
  l `gsymeq` (CBool True) = l
  l `gsymeq` (CBool False) = nots l
  l `gsymeq` r
    | l == r = CBool True
    | otherwise = Equal l r

instance GSOrd SBool SBool where
  l `gsymle` r = nots l ||~ r
  l `gsymlt` r = nots l &&~ r
  l `gsymge` r = l ||~ nots r
  l `gsymgt` r = l &&~ nots r
  gsymCompare l r =
    mrgIf
      (nots l &&~ r)
      (mrgReturn LT)
      (mrgIf (l `gsymeq` r) (mrgReturn EQ) (mrgReturn GT))

instance IsString SBool where
  fromString = ssymb

instance Solvable Bool SBool where
  conc = CBool
  concView (CBool x) = Just x
  concView _ = Nothing
  ssymb = SSBool
  sinfosymb = ISSBool
  isymb = ISBool
  iinfosymb = IISBool

instance ITEOp SBool SBool where
  ites (CBool True) l _ = l
  ites (CBool False) _ r = r
  ites cond l r = ITE cond l r

instance LogicalOp SBool where
  CBool True ||~ _ = CBool True
  CBool False ||~ r = r
  _ ||~ CBool True = CBool True
  l ||~ CBool False = l
  l ||~ r = Or l r
  CBool False &&~ _ = CBool False
  CBool True &&~ r = r
  _ &&~ CBool False = CBool False
  l &&~ CBool True = l
  l &&~ r = And l r
  nots (CBool x) = CBool (not x)
  nots v = Not v

instance SymBoolOp SBool

data Symbol where
  SSymbol :: String -> Symbol
  ISSymbol :: (Typeable a, Show a, Eq a, Hashable a) => String -> a -> Symbol
  ISymbol :: String -> Int -> Symbol
  IISymbol :: (Typeable a, Show a, Eq a, Hashable a) => String -> Int -> a -> Symbol

-- deriving (Generic, Show, Eq, Hashable)

instance Show Symbol where
  show (SSymbol s) = "SSymbol " ++ s
  show (ISSymbol s info) = "ISSymbol " ++ s ++ " " ++ show info
  show (ISymbol s i) = "ISymbol " ++ show i ++ " " ++ s
  show (IISymbol s i info) = "IISymbol " ++ show i ++ " " ++ s ++ " " ++ show info

instance Eq Symbol where
  SSymbol s1 == SSymbol s2 = s1 == s2
  ISSymbol s1 (info1 :: info1) == ISSymbol s2 (info2 :: info2) =
    case eqT @info1 @info2 of
      Just Refl -> s1 == s2 && info1 == info2
      _ -> False
  ISymbol s1 i1 == ISymbol s2 i2 = i1 == i2 && s1 == s2
  IISymbol s1 i1 (info1 :: info1) == IISymbol s2 i2 (info2 :: info2) =
    case eqT @info1 @info2 of
      Just Refl -> i1 == i2 && s1 == s2 && info1 == info2
      _ -> False
  _ == _ = False

instance Hashable Symbol where
  s `hashWithSalt` SSymbol s1 = s `hashWithSalt` s1
  s `hashWithSalt` ISSymbol s1 info = s `hashWithSalt` s1 `hashWithSalt` info
  s `hashWithSalt` ISymbol i1 s1 = s `hashWithSalt` i1 `hashWithSalt` s1
  s `hashWithSalt` IISymbol i1 s1 info1 = s `hashWithSalt` i1 `hashWithSalt` s1 `hashWithSalt` info1

instance GExtractSymbolics (S.HashSet Symbol) SBool where
  gextractSymbolics = go S.empty
    where
      go s (CBool _) = s
      go s (SSBool sym) = S.insert (SSymbol sym) s
      go s (ISSBool sym info) = S.insert (ISSymbol sym info) s
      go s (ISBool sym i) = S.insert (ISymbol sym i) s
      go s (IISBool sym i info) = S.insert (IISymbol sym i info) s
      go s (Or l r) = go (go s l) r
      go s (And l r) = go (go s l) r
      go s (Not v) = go s v
      go s (Equal l r) = go (go s l) r
      go s (ITE c l r) = go (go (go s l) r) c

instance ToCon SBool Bool where
  toCon (CBool v) = Just v
  toCon _ = Nothing

instance ToCon SBool SBool where
  toCon = Just

instance ToSym Bool SBool where
  toSym = CBool

instance ToSym SBool SBool where
  toSym = id

instance GenSym SBool () SBool

instance GenSymSimple () SBool where
  simpleFresh _ = do
    ident <- getFreshIdent
    FreshIndex i <- nextFreshIndex
    case ident of
      FreshIdent s -> return $ ISBool s i
      FreshIdentWithInfo s info -> return $ IISBool s i info

instance GenSym SBool SBool SBool

instance GenSymSimple SBool SBool where
  simpleFresh _ = simpleFresh ()
