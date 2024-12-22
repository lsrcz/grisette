{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.Ctor.UnifiedConstructor
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Ctor.UnifiedConstructor
  ( makeUnifiedCtorWith,
    makePrefixedUnifiedCtor,
    makeNamedUnifiedCtor,
    makeUnifiedCtor,
  )
where

import Control.Monad (join, replicateM, when, zipWithM)
import Data.Maybe (catMaybes)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable, Mergeable1, Mergeable2)
import Grisette.Internal.TH.Ctor.Common
  ( decapitalizeTransformer,
    prefixTransformer,
    withNameTransformer,
  )
import Grisette.Internal.TH.GADT.Common (ctxForVar)
import Grisette.Internal.TH.Util (constructorInfoToType, putHaddock, tvIsMode)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag)
import Grisette.Unified.Internal.UnifiedData
  ( GetData,
    UnifiedData,
    wrapData,
  )
import Language.Haskell.TH (conT, pprint, varT)
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName),
    DatatypeInfo (datatypeCons, datatypeVars),
    reifyDatatype,
    tvKind,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrSpec, kindedTVSpecified)
import Language.Haskell.TH.Lib (appE, appTypeE, lamE, varE, varP)
import Language.Haskell.TH.Syntax
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, SigD),
    Exp (ConE),
    Name,
    Pred,
    Q,
    Type (AppT, ArrowT, ConT, ForallT, VarT),
    mkName,
    newName,
  )

-- | Generate smart constructors to create unified values with provided name
-- transformer.
--
-- For a type @T mode a b c@ with constructors @T1@, @T2@, etc., this function
-- will generate smart constructors with the name transformed, e.g., given the
-- name transformer @(\name -> "mk" ++ name)@, it will generate @mkT1@, @mkT2@,
-- @mkT2@, etc.
--
-- The generated smart constructors will contruct values of type
-- @GetData mode (T mode a b c)@.
makeUnifiedCtorWith :: [Name] -> (String -> String) -> Name -> Q [Dec]
makeUnifiedCtorWith = withNameTransformer . makeNamedUnifiedCtor

-- | Generate smart constructors to create unified values.
--
-- For a type @T mode a b c@ with constructors @T1@, @T2@, etc., this function
-- will generate smart constructors with the given prefix, e.g., @mkT1@, @mkT2@,
-- etc.
--
-- The generated smart constructors will contruct values of type
-- @GetData mode (T mode a b c)@.
makePrefixedUnifiedCtor ::
  [Name] ->
  -- | Prefix for generated wrappers
  String ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makePrefixedUnifiedCtor modeCtx =
  makeUnifiedCtorWith modeCtx . prefixTransformer

-- | Generate smart constructors to create unified values.
--
-- For a type @T mode a b c@ with constructors @T1@, @T2@, etc., this function
-- will generate smart constructors with the names decapitalized, e.g.,
-- @t1@, @t2@, etc.
--
-- The generated smart constructors will contruct values of type
-- @GetData mode (T mode a b c)@.
makeUnifiedCtor ::
  [Name] ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makeUnifiedCtor modeCtx = makeUnifiedCtorWith modeCtx decapitalizeTransformer

-- | Generate smart constructors to create unified values.
--
-- For a type @T mode a b c@ with constructors @T1@, @T2@, etc., this function
-- will generate smart constructors with the given names.
--
-- The generated smart constructors will contruct values of type
-- @GetData mode (T mode a b c)@.
makeNamedUnifiedCtor ::
  [Name] ->
  -- | Names for generated wrappers
  [String] ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makeNamedUnifiedCtor modeCtx names typName = do
  d <- reifyDatatype typName
  let constructors = datatypeCons d
  when (length names /= length constructors) $
    fail "Number of names does not match the number of constructors"
  let modeVars = filter ((== ConT ''EvalModeTag) . tvKind) (datatypeVars d)
  -- when (length modeVars /= 1) $
  --  fail "Expected exactly one EvalModeTag variable in the datatype."
  case modeVars of
    [mode] -> do
      ds <-
        zipWithM
          (mkSingleWrapper modeCtx d Nothing $ VarT $ tvName mode)
          names
          constructors
      return $ join ds
    [] -> do
      n <- newName "mode"
      let newBndr = kindedTVSpecified n (ConT ''EvalModeTag)
      ds <-
        zipWithM
          (mkSingleWrapper modeCtx d (Just newBndr) (VarT n))
          names
          constructors
      return $ join ds
    _ -> fail "Expected one or zero EvalModeTag variable in the datatype."

augmentFinalType :: Type -> Type -> Q ([Pred], Type)
augmentFinalType mode (AppT a@(AppT ArrowT _) t) = do
  (pred, ret) <- augmentFinalType mode t
  return (pred, AppT a ret)
augmentFinalType mode t = do
  r <- [t|GetData $(return mode) $(return t)|]
  predu <- [t|UnifiedData $(return mode) $(return t)|]
  return ([predu], r)

augmentConstructorType ::
  [Name] -> Maybe TyVarBndrSpec -> Type -> Type -> Q Type
augmentConstructorType
  modeCtx
  freshModeBndr
  mode
  (ForallT tybinders ctx ty1) = do
    (preds, augmentedTyp) <- augmentFinalType mode ty1
    let modeBndrsInForall = filter tvIsMode tybinders
    mergeablePreds <-
      catMaybes
        <$> traverse
          (ctxForVar $ ConT <$> [''Mergeable, ''Mergeable1, ''Mergeable2])
          tybinders
    modePred <-
      case (modeBndrsInForall, freshModeBndr) of
        ([bndr], Nothing) ->
          traverse (\nm -> [t|$(conT nm) $(varT $ tvName bndr)|]) modeCtx
        ([], Just bndr) ->
          traverse (\nm -> [t|$(conT nm) $(varT $ tvName bndr)|]) modeCtx
        _ -> fail "Unsupported constructor type."
    case freshModeBndr of
      Just bndr -> do
        return $
          ForallT
            (bndr : tybinders)
            (modePred ++ mergeablePreds ++ preds ++ ctx)
            augmentedTyp
      Nothing ->
        return $
          ForallT
            tybinders
            (modePred ++ mergeablePreds ++ preds ++ ctx)
            augmentedTyp
augmentConstructorType _ freshModeBndr mode ty = do
  (preds, augmentedTyp) <- augmentFinalType mode ty
  case freshModeBndr of
    Just bndr -> return $ ForallT [bndr] (preds) augmentedTyp
    Nothing ->
      fail $
        "augmentConstructorType: unsupported constructor type: " ++ pprint ty

augmentExpr :: Type -> Int -> Exp -> Q Exp
augmentExpr mode n f = do
  xs <- replicateM n (newName "x")
  let args = map varP xs
  lamE
    args
    ( ( appE
          (appTypeE [|wrapData|] (return mode))
          (foldl appE (return f) (map varE xs))
      )
    )

mkSingleWrapper ::
  [Name] ->
  DatatypeInfo ->
  Maybe TyVarBndrSpec ->
  Type ->
  String ->
  ConstructorInfo ->
  Q [Dec]
mkSingleWrapper modeCtx dataType freshModeBndr mode name info = do
  constructorTyp <- constructorInfoToType dataType info
  augmentedTyp <-
    augmentConstructorType modeCtx freshModeBndr mode constructorTyp
  let oriName = constructorName info
  let retName = mkName name
  expr <- augmentExpr mode (length $ constructorFields info) (ConE oriName)
  putHaddock retName $
    "Smart constructor for v'"
      <> show oriName
      <> "' to construct unified value."
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
