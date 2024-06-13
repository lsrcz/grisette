{-# LANGUAGE TemplateHaskell #-}

module Grisette.Unified.Internal.TH.UnifiedConstructor
  ( mkUnifiedConstructor,
    mkUnifiedConstructor',
  )
where

import Control.Monad (join, replicateM, when, zipWithM)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.TH.Util (constructorInfoToType, occName)
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode)
import Grisette.Unified.Internal.IsMode (IsMode)
import Grisette.Unified.Internal.UnifiedData
  ( GetData,
    UnifiedData,
    UnifiedDataImpl (wrapData),
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName),
    DatatypeInfo (datatypeCons, datatypeVars),
    reifyDatatype,
    tvKind,
    tvName,
  )
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

mkUnifiedConstructor ::
  -- | Prefix for generated wrappers
  String ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
mkUnifiedConstructor prefix typName = do
  d <- reifyDatatype typName
  let constructorNames = occName . constructorName <$> datatypeCons d
  mkUnifiedConstructor' ((prefix ++) <$> constructorNames) typName

mkUnifiedConstructor' ::
  -- | Names for generated wrappers
  [String] ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
mkUnifiedConstructor' names typName = do
  d <- reifyDatatype typName
  let constructors = datatypeCons d
  when (length names /= length constructors) $
    fail "Number of names does not match the number of constructors"
  let modeVars = filter ((== ConT ''EvaluationMode) . tvKind) (datatypeVars d)
  when (length modeVars /= 1) $
    fail "Expected exactly one EvaluationMode variable in the datatype."
  case modeVars of
    [mode] -> do
      ds <- zipWithM (mkSingleWrapper d $ VarT $ tvName mode) names constructors
      return $ join ds
    _ -> fail "Expected exactly one EvaluationMode variable in the datatype."

augmentFinalType :: Type -> Type -> Q ([Pred], Type)
augmentFinalType mode (AppT a@(AppT ArrowT _) t) = do
  (pred, ret) <- augmentFinalType mode t
  return (pred, AppT a ret)
augmentFinalType mode t = do
  r <- [t|GetData $(return mode) $(return t)|]
  pred <- [t|Mergeable $(return t)|]
  predu <- [t|UnifiedData $(return mode) $(return t)|]
  return ([pred, predu], r)

augmentConstructorType :: Type -> Type -> Q Type
augmentConstructorType mode (ForallT tybinders ctx ty1) = do
  (preds, augmentedTyp) <- augmentFinalType mode ty1
  ismode <- [t|IsMode $(return mode)|]
  return $ ForallT tybinders (ismode : preds ++ ctx) augmentedTyp
augmentConstructorType _ _ =
  fail
    "augmentConstructorType: unsupported constructor, must be a forall type."

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

mkSingleWrapper :: DatatypeInfo -> Type -> String -> ConstructorInfo -> Q [Dec]
mkSingleWrapper dataType mode name info = do
  constructorTyp <- constructorInfoToType dataType info
  augmentedTyp <- augmentConstructorType mode constructorTyp
  let oriName = constructorName info
  let retName = mkName name
  expr <- augmentExpr mode (length $ constructorFields info) (ConE oriName)
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
