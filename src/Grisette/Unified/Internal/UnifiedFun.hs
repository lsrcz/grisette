{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedFun
  ( UnifiedFunConstraint,
    UnifiedFun (..),
    unifiedFunInstanceName,
    genUnifiedFunInstance,
    GetFun2,
    GetFun3,
    GetFun4,
    GetFun5,
    GetFun6,
    GetFun7,
    GetFun8,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (Foldable (foldl'))
import Data.Hashable (Hashable)
import qualified Data.Kind
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Data.Class.EvalSym (EvalSym)
import Grisette.Internal.Core.Data.Class.ExtractSym (ExtractSym)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType), Function)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.PPrint (PPrint)
import Grisette.Internal.Core.Data.Class.SubstSym (SubstSym)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP (SymFP)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Grisette.Unified.Internal.Theories
  ( TheoryToUnify (UAlgReal, UBool, UFP, UFun, UIntN, UInteger, UWordN),
  )
import Grisette.Unified.Internal.UnifiedAlgReal (GetAlgReal)
import Grisette.Unified.Internal.UnifiedBV (UnifiedBVImpl (GetIntN, GetWordN))
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.UnifiedFP (UnifiedFPImpl (GetFP))
import Grisette.Unified.Internal.UnifiedInteger (GetInteger)
import Language.Haskell.TH
  ( DecsQ,
    Pred,
    Q,
    TyLit (NumTyLit),
    Type (AppT, ConT, ForallT, LitT, VarT),
    appT,
    classD,
    conT,
    instanceD,
    mkName,
    newName,
    promotedT,
    varT,
  )
import qualified Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr
  ( kindedTV,
    mapTVFlag,
    specifiedSpec,
    tvName,
  )
import Language.Haskell.TH.Syntax (Lift)

#if MIN_VERSION_template_haskell(2,21,0)
type TyVarBndrVis = Language.Haskell.TH.TyVarBndrVis
#elif MIN_VERSION_template_haskell(2,17,0)
type TyVarBndrVis = Language.Haskell.TH.TyVarBndr ()
#else
type TyVarBndrVis = Language.Haskell.TH.TyVarBndr
#endif

-- | Provide unified function types.
class UnifiedFun (mode :: EvalModeTag) where
  -- | Get a unified function type. Resolves to t'Grisette.SymPrim.=->' in 'Con'
  -- mode, and t'Grisette.SymPrim.=~>' in 'Sym' mode.
  type
    GetFun mode =
      (fun :: Data.Kind.Type -> Data.Kind.Type -> Data.Kind.Type) | fun -> mode

instance UnifiedFun 'C where
  type GetFun 'C = (=->)

instance UnifiedFun 'S where
  type GetFun 'S = (=~>)

-- | The unified function type with 2 arguments.
type GetFun2 mode a b = GetFun mode a b

-- | The unified function type with 3 arguments.
type GetFun3 mode a b c = GetFun mode a (GetFun mode b c)

-- | The unified function type with 4 arguments.
type GetFun4 mode a b c d = GetFun mode a (GetFun mode b (GetFun mode c d))

-- | The unified function type with 5 arguments.
type GetFun5 mode a b c d e =
  GetFun mode a (GetFun mode b (GetFun mode c (GetFun mode d e)))

-- | The unified function type with 6 arguments.
type GetFun6 mode a b c d e f =
  GetFun
    mode
    a
    (GetFun mode b (GetFun mode c (GetFun mode d (GetFun mode e f))))

-- | The unified function type with 7 arguments.
type GetFun7 mode a b c d e f g =
  GetFun
    mode
    a
    ( GetFun
        mode
        b
        (GetFun mode c (GetFun mode d (GetFun mode e (GetFun mode f g))))
    )

-- | The unified function type with 8 arguments.
type GetFun8 mode a b c d e f g h =
  GetFun
    mode
    a
    ( GetFun
        mode
        b
        ( GetFun
            mode
            c
            (GetFun mode d (GetFun mode e (GetFun mode f (GetFun mode g h))))
        )
    )

-- | The constraint for a unified function.
type UnifiedFunConstraint mode a b ca cb sa sb =
  ( Eq (GetFun mode a b),
    EvalSym (GetFun mode a b),
    ExtractSym (GetFun mode a b),
    PPrint (GetFun mode a b),
    Hashable (GetFun mode a b),
    Lift (GetFun mode a b),
    Mergeable (GetFun mode a b),
    NFData (GetFun mode a b),
    Show (GetFun mode a b),
    SubstSym (GetFun mode a b),
    ToCon (GetFun mode a b) (ca =-> cb),
    ToCon (sa =~> sb) (GetFun mode a b),
    ToSym (GetFun mode a b) (sa =~> sb),
    ToSym (ca =-> cb) (GetFun mode a b),
    Function (GetFun mode a b) a b,
    Apply (GetFun mode a b),
    FunType (GetFun mode a b) ~ (a -> b)
  )

genInnerUnifiedFunInstance ::
  String ->
  TyVarBndrVis ->
  [Pred] ->
  [TyVarBndrVis] ->
  [(Type, Type, Type)] ->
  DecsQ
genInnerUnifiedFunInstance nm mode preds bndrs tys = do
  x <- classD (goPred tys) (mkName nm) (mode : bndrs) [] []
  dc <-
    instanceD
      (return preds)
      (applyTypeList (promotedT 'C : additionalTypes))
      []
  ds <-
    instanceD
      (return preds)
      (applyTypeList (promotedT 'S : additionalTypes))
      []
  return [x, dc, ds]
  where
    additionalTypes = (varT . tvName) <$> bndrs
    applyTypeList = foldl appT (conT (mkName nm))
    goPred :: [(Type, Type, Type)] -> Q [Pred]
    goPred [] = fail "Empty list of function types, at least 2."
    goPred [_] = return []
    goPred (x : xs) = do
      p1 <- pred x xs
      pr <- goPred xs
      return $ p1 : pr
    listTys :: [(Type, Type, Type)] -> Q (Type, Type, Type)
    listTys [] = fail "Should not happen"
    listTys [(u, c, s)] = return (u, c, s)
    listTys ((u, c, s) : xs) = do
      (u', c', s') <- listTys xs
      return
        ( AppT (AppT (AppT (ConT ''GetFun) (VarT $ tvName mode)) u) u',
          AppT (AppT (ConT ''(=->)) c) c',
          AppT (AppT (ConT ''(=~>)) s) s'
        )
    pred (ua, ca, sa) l = do
      (ub, cb, sb) <- listTys l
      [t|
        UnifiedFunConstraint
          $(return (VarT $ tvName mode))
          $(return ua)
          $(return ub)
          $(return ca)
          $(return cb)
          $(return sa)
          $(return sb)
        |]

genOuterUnifiedFunInstance ::
  String -> String -> TyVarBndrVis -> [Pred] -> [TyVarBndrVis] -> DecsQ
genOuterUnifiedFunInstance nm innerName mode preds bndrs = do
  let bndrs' = mapTVFlag (const specifiedSpec) <$> bndrs
  x <-
    classD
      ( return
          [ ForallT bndrs' preds $
              foldl' AppT (ConT $ mkName innerName) $
                VarT . tvName <$> mode : bndrs
          ]
      )
      (mkName nm)
      [mode]
      []
      []
  dc <-
    instanceD
      (return [])
      (appT (conT $ mkName nm) (promotedT 'C))
      []
  ds <-
    instanceD
      (return [])
      (appT (conT $ mkName nm) (promotedT 'S))
      []
  return [x, dc, ds]

-- | Generate unified function instance names.
unifiedFunInstanceName :: String -> [TheoryToUnify] -> String
unifiedFunInstanceName prefix theories =
  prefix ++ "Fun" ++ (concatMap show theories)

-- | Generate unified function instances.
genUnifiedFunInstance :: String -> [TheoryToUnify] -> DecsQ
genUnifiedFunInstance prefix theories = do
  modeName <- newName "mode"
  let modeType = VarT modeName
  allArgs <- traverse (genArgs modeType) theories
  let baseName = unifiedFunInstanceName prefix theories
  rinner <-
    genInnerUnifiedFunInstance
      baseName
      (kindedTV modeName (ConT ''EvalModeTag))
      (concatMap (\(_, p, _, _, _) -> p) allArgs)
      (concatMap (\(t, _, _, _, _) -> t) allArgs)
      ((\(_, _, u, c, s) -> (u, c, s)) <$> allArgs)
  router <-
    if all (\(bndr, _, _, _, _) -> null bndr) allArgs
      then return []
      else
        genOuterUnifiedFunInstance
          ("All" ++ baseName)
          baseName
          (kindedTV modeName (ConT ''EvalModeTag))
          (concatMap (\(_, p, _, _, _) -> p) allArgs)
          (concatMap (\(t, _, _, _, _) -> t) allArgs)
  return $ rinner ++ router
  where
    genArgs ::
      Type -> TheoryToUnify -> Q ([TyVarBndrVis], [Pred], Type, Type, Type)
    genArgs mode UBool =
      return
        ( [],
          [],
          AppT (ConT ''GetBool) mode,
          ConT ''Bool,
          ConT ''SymBool
        )
    genArgs mode UIntN = do
      n <- newName "n"
      let nType = VarT n
      return
        ( [kindedTV n (ConT ''Nat)],
          [ AppT (ConT ''KnownNat) nType,
            AppT (AppT (ConT ''(<=)) (LitT $ NumTyLit 1)) nType
          ],
          AppT (AppT (ConT ''GetIntN) mode) nType,
          AppT (ConT ''IntN) nType,
          AppT (ConT ''SymIntN) nType
        )
    genArgs mode UWordN = do
      n <- newName "n"
      let nType = VarT n
      return
        ( [kindedTV n (ConT ''Nat)],
          [ AppT (ConT ''KnownNat) nType,
            AppT (AppT (ConT ''(<=)) (LitT $ NumTyLit 1)) nType
          ],
          AppT (AppT (ConT ''GetWordN) mode) nType,
          AppT (ConT ''WordN) nType,
          AppT (ConT ''SymWordN) nType
        )
    genArgs mode UInteger =
      return
        ( [],
          [],
          AppT (ConT ''GetInteger) mode,
          ConT ''Integer,
          ConT ''SymInteger
        )
    genArgs mode UAlgReal =
      return
        ( [],
          [],
          AppT (ConT ''GetAlgReal) mode,
          ConT ''AlgReal,
          ConT ''SymAlgReal
        )
    genArgs mode UFP = do
      eb <- newName "eb"
      sb <- newName "sb"
      let ebType = VarT eb
      let sbType = VarT sb
      return
        ( [kindedTV eb (ConT ''Nat), kindedTV sb (ConT ''Nat)],
          [AppT (AppT (ConT ''ValidFP) ebType) sbType],
          AppT (AppT (AppT (ConT ''GetFP) mode) ebType) sbType,
          AppT (AppT (ConT ''FP) ebType) sbType,
          AppT (AppT (ConT ''SymFP) ebType) sbType
        )
    genArgs _ UFun {} = fail "UFun cannot be nested."
