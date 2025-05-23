{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Unified.EvalMode
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.EvalMode
  ( EvalModeBase,
    EvalModeInteger,
    EvalModeBV,
    EvalModeFP,
    EvalModeAlgReal,
    EvalModeAll,
    MonadEvalModeAll,
    genEvalMode,
  )
where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Internal.Internal.Decl.Unified.BVFPConversion
  ( AllUnifiedBVFPConversion,
  )
import Grisette.Internal.Internal.Decl.Unified.FPFPConversion
  ( AllUnifiedFPFPConversion,
  )
import Grisette.Internal.Internal.Decl.Unified.UnifiedBV (AllUnifiedBV)
import Grisette.Internal.Internal.Decl.Unified.UnifiedBool
  ( UnifiedBool (GetBool),
  )
import Grisette.Internal.Internal.Decl.Unified.UnifiedFP (AllUnifiedFP)
import Grisette.Internal.Unified.BVBVConversion (AllUnifiedBVBVConversion)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Grisette.Internal.Unified.Theories
  ( TheoryToUnify (UAlgReal, UFP, UFun, UIntN, UInteger, UWordN),
    isUFun,
  )
import Grisette.Internal.Unified.UnifiedAlgReal (UnifiedAlgReal)
import Grisette.Internal.Unified.UnifiedData (AllUnifiedData, UnifiedDataBase)
import Grisette.Internal.Unified.UnifiedFun
  ( genUnifiedFunInstance,
    unifiedFunInstanceName,
  )
import Grisette.Internal.Unified.UnifiedInteger (UnifiedInteger)
import Grisette.Internal.Unified.UnifiedPrim (UnifiedBasicPrim)
import Grisette.Internal.Unified.Util (DecideEvalMode)
import Language.Haskell.TH
  ( DecsQ,
    Type (AppT, ArrowT, ConT, StarT, VarT),
    appT,
    classD,
    conT,
    instanceD,
    kindedTV,
    mkName,
    newName,
    promotedT,
    tySynD,
    varT,
  )

-- | Provide the constraint that the mode is a valid evaluation mode, and
-- provides the support for 'GetBool' and 'Grisette.Internal.Unified.GetData'.
--
-- For compilers prior to GHC 9.2.1, see the notes for 'EvalModeAll'.
class
  ( DecideEvalMode mode,
    UnifiedBool mode,
    UnifiedBasicPrim mode (GetBool mode),
    AllUnifiedData mode,
    UnifiedDataBase mode
  ) =>
  EvalModeBase mode

-- | Provide the support for 'Grisette.Internal.Unified.GetIntN',
-- 'Grisette.Internal.Unified.GetWordN', 'Grisette.Internal.Unified.GetSomeIntN', and
-- 'Grisette.Internal.Unified.GetSomeWordN'.
--
-- For compilers prior to GHC 9.2.1, see the notes for 'EvalModeAll'.
class (AllUnifiedBV mode, AllUnifiedBVBVConversion mode) => EvalModeBV mode

-- | Provide the support for 'Grisette.Internal.Unified.GetInteger'.
--
-- For compilers prior to GHC 9.2.1, see the notes for 'EvalModeAll'.
type EvalModeInteger = UnifiedInteger

-- | Provide the support for 'Grisette.Internal.Unified.GetFP' and
-- 'Grisette.Internal.Unified.GetFPRoundingMode'.
--
-- For compilers prior to GHC 9.2.1, see the notes for 'EvalModeAll'.
class
  ( AllUnifiedFP mode,
    AllUnifiedFPFPConversion mode,
    AllUnifiedBVFPConversion mode
  ) =>
  EvalModeFP mode

-- | Provide the support for 'Grisette.Internal.Unified.GetAlgReal'.
--
-- For compilers prior to GHC 9.2.1, see the notes for 'EvalModeAll'.
type EvalModeAlgReal = UnifiedAlgReal

-- | A constraint that specifies that the mode is valid, and provide all the
-- corresponding constraints for the operaions for the types.
--
-- Note for users with GHC prior to 9.2.1: the GHC compiler isn't able to
-- resolve the operations for sized bitvectors and data types. In this case,
-- you may need to provide `Grisette.Internal.Unified.UnifiedBV.UnifiedBV`,
-- `Grisette.Internal.Unified.UnifiedBV.SafeUnifiedBV`,
-- `Grisette.Internal.Unified.UnifiedBV.SafeUnifiedSomeBV`, and
-- `Grisette.Internal.Unified.UnifiedData.UnifiedData` constraints manually.
--
-- For example, the following code is valid for GHC 9.2.1 and later:
--
-- > fbv ::
-- >   forall mode n.
-- >   (EvalMode mode, KnownNat n, 1 <= n) =>
-- >   GetIntN mode n ->
-- >   GetIntN mode n ->
-- >   GetIntN mode n
-- > fbv l r =
-- >   mrgIte @mode
-- >     (l .== r)
-- >     (l + r)
-- >     (symIte @mode (l .< r) l r)
--
-- But with older GHCs, you need to write:
--
-- > fbv ::
-- >   forall mode n.
-- >   (EvalMode mode, KnownNat n, 1 <= n, UnifiedBV mode n) =>
-- >   GetIntN mode n ->
-- >   GetIntN mode n ->
-- >   GetIntN mode n
-- > fbv l r =
-- >   mrgIte @mode
-- >     (l .== r)
-- >     (l + r)
-- >     (symIte @mode (l .< r) l r)
class
  ( EvalModeBase mode,
    EvalModeInteger mode,
    EvalModeAlgReal mode,
    EvalModeBV mode,
    EvalModeFP mode
  ) =>
  EvalModeAll mode

-- | A constraint that specifies that the mode is valid, and provide all the
-- corresponding constraints for the operations for the types.
--
-- This also provide the branching constraints for the monad, and the safe
-- operations: for example, 'Grisette.Internal.Unified.SafeUnifiedInteger' provides
-- 'Grisette.safeDiv' for the integer type with in @ExceptT ArithException m@.
--
-- For users with GHC prior to 9.2.1, see notes in 'EvalModeAll'.
type MonadEvalModeAll mode m =
  ( EvalModeAll mode,
    Monad m,
    TryMerge m,
    UnifiedBranching mode m
  )

-- | This template haskell function generates an EvalMode constraint on demand.
--
-- For example, if in your system, you are only working on bit-vectors and
-- booleans, but not floating points, integers, or real numbers, you can use
-- this function to generate a constraint that only includes the necessary
-- constraints:
--
-- > genEvalMode "MyEvalMode" [UWordN, UIntN, UBool]
-- > f :: MyEvalMode mode => GetBool mode -> GetWordN mode 8 -> GetWordN mode 8
-- > f = ...
--
-- This may help with faster compilation times.
--
-- Another usage of this custom constraint is to working with uninterpreted
-- functions. The uninterpreted functions aren't available even with
-- 'EvalModeAll', and is only available with the constraint generated by this
-- function. Note that you need to explicitly list all the uninterpreted
-- function types you need in your system.
--
-- > genEvalMode "MyEvalModeUF" [UFun [UWordN, UIntN], UFun [UBool, UBool, UWordN]]
--
-- This will give us a constraint that allows us to work with booleans and
-- bit-vectors, and also the uninterpreted functions that
--
-- * maps an unsigned bit-vector (any bitwidth) to an unsigned integer (any
--   bitwidth), and
-- * maps two booleans to an unsigned bit-vector (any bitwidth).
--
-- You can then use them in your code like this:
--
-- > f :: MyEvalModeUF mode => GetFun mode (GetWordN mode 8) (GetIntN mode 8) -> GetIntN mode 8
-- > f fun = f # 1
--
-- The function will also provide the constraint @MonadMyEvalModeUF@, which
-- includes the constraints for the monad and the unified branching, similar to
-- 'MonadEvalModeAll'.
--
-- For compilers older than GHC 9.2.1, see the notes for 'EvalModeAll'. This
-- function will also generate constraints like @MyEvalModeUFFunUWordNUIntN@,
-- which can be used to resolve the constraints for older compilers.
--
-- The naming conversion is the concatenation of the three parts:
--
-- * The base name provided by the user (i.e., @MyEvalModeUF@),
-- * @Fun@,
-- * The concatenation of all the types in the uninterpreted function (i.e.,
--   @UWordNUIntN@).
--
-- The arguments to the type class is as follows:
--
-- * The first argument is the mode,
-- * The second to the end arguments are the natural number arguments for all
--   the types. Here the second argument is the bitwidth of the unsigned
--   bit-vector argument, and the third argument is the bitwidth of the signed
--   bit-vector result.
genEvalMode :: String -> [TheoryToUnify] -> DecsQ
genEvalMode nm theories = do
  modeName <- newName "mode"
  let modeType = VarT modeName
  baseConstraint <- [t|EvalModeBase $(return modeType)|]
  basicConstraints <- concat <$> traverse (nonFuncConstraint modeType) nonFuncs
  funcInstances <- concat <$> traverse (genUnifiedFunInstance nm) funcs
  let instanceNames = ("All" ++) . unifiedFunInstanceName nm <$> funcs
  funcConstraints <- traverse (genFunConstraint (return modeType)) instanceNames
  r <-
    classD
      (return $ baseConstraint : basicConstraints ++ funcConstraints)
      (mkName nm)
      [kindedTV modeName (ConT ''EvalModeTag)]
      []
      []
  rc <- instanceD (return []) (appT (conT $ mkName nm) (promotedT 'C)) []
  rs <- instanceD (return []) (appT (conT $ mkName nm) (promotedT 'S)) []
  m <- newName "m"
  let mType = varT m
  monad <-
    tySynD
      (mkName $ "Monad" ++ nm)
      [ kindedTV modeName (ConT ''EvalModeTag),
        kindedTV m (AppT (AppT ArrowT StarT) StarT)
      ]
      [t|
        ( $(appT (conT $ mkName nm) (return modeType)),
          Monad $mType,
          TryMerge $mType,
          UnifiedBranching $(return modeType) $mType
        )
        |]
  return $ funcInstances ++ [r, rc, rs, monad]
  where
    nonFuncs =
      nub $
        (\x -> if x == UIntN then UWordN else x)
          <$> filter (not . isUFun) (theories ++ concat funcs)
    funcs =
      nub $
        mapMaybe
          ( \case
              UFun x -> Just x
              _ -> Nothing
          )
          theories
    nonFuncConstraint mode UInteger =
      (: []) <$> [t|EvalModeInteger $(return mode)|]
    nonFuncConstraint mode UAlgReal =
      (: []) <$> [t|EvalModeAlgReal $(return mode)|]
    nonFuncConstraint mode UWordN =
      (: []) <$> [t|EvalModeBV $(return mode)|]
    nonFuncConstraint mode UFP = (: []) <$> [t|EvalModeFP $(return mode)|]
    nonFuncConstraint _ _ = return []
    genFunConstraint mode name = appT (conT (mkName name)) mode
