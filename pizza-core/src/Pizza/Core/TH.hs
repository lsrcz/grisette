{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Pizza.Core.TH
  ( makeUnionMWrapper,
    makeUnionMWrapper',
  )
where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Pizza.Core.THCompat

-- | Generate constructor wrappers that wraps the result in a union-like monad with provided names.
--
-- > $(makeUnionMWrapper' ["uTuple2"] ''(,))
--
-- generates
--
-- > uTuple2 :: (SymBoolOp bool, Monad u, Mergeable bool t1, Mergeable bool t2, MonadUnion bool u) => t1 -> t2 -> u (t1, t2)
-- > uTuple2 = \v1 v2 -> mrgSingle (v1, v2)
makeUnionMWrapper' ::
  -- | Names for generated wrappers
  [String] ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makeUnionMWrapper' names typName = do
  constructors <- getConstructors typName
  when (length names /= length constructors) $
    fail "Number of names does not match the number of constructors"
  ds <- zipWithM mkSingleWrapper names constructors
  return $ join ds

occName :: Name -> String
occName (Name (OccName name) _) = name

getConstructorName :: Con -> Q String
getConstructorName (NormalC name _) = return $ occName name
getConstructorName (RecC name _) = return $ occName name
getConstructorName InfixC {} =
  fail "You should use makeUnionMWrapper' to manually provide the name for infix constructors"
getConstructorName (ForallC _ _ c) = getConstructorName c
getConstructorName (GadtC [name] _ _) = return $ occName name
getConstructorName (RecGadtC [name] _ _) = return $ occName name
getConstructorName c = fail $ "Unsupported constructor at this time: " ++ pprint c

getConstructors :: Name -> Q [Con]
getConstructors typName = do
  d <- reify typName
  case d of
    TyConI (DataD _ _ _ _ constructors _) -> return constructors
    TyConI (NewtypeD _ _ _ _ constructor _) -> return [constructor]
    _ -> fail $ "Unsupported declaration: " ++ pprint d

-- | Generate constructor wrappers that wraps the result in a union-like monad.
--
-- > $(makeUnionMWrapper "u" ''Maybe)
--
-- generates
--
-- > uNothing :: (SymBoolOp bool, Monad u, Mergeable bool t, MonadUnion bool u) => u (Maybe t)
-- > uNothing = mrgSingle Nothing
-- > uJust :: (SymBoolOp bool, Monad u, Mergeable bool t, MonadUnion bool u) => t -> u (Maybe t)
-- > uJust = \x -> mrgSingle (Just x)
makeUnionMWrapper ::
  -- | Prefix for generated wrappers
  String ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makeUnionMWrapper prefix typName = do
  constructors <- getConstructors typName
  constructorNames <- mapM getConstructorName constructors
  makeUnionMWrapper' ((prefix ++) <$> constructorNames) typName

augmentNormalCExpr :: Int -> Exp -> Q Exp
augmentNormalCExpr n f = do
  xs <- replicateM n (newName "x")
  let args = map VarP xs
  mrgSingleFunc <- [|mrgSingle|]
  return $
    LamE
      args
      ( AppE mrgSingleFunc $
          foldl AppE f (map VarE xs)
      )

augmentNormalCType :: Type -> Q Type
augmentNormalCType (ForallT tybinders ctx ty1) = do
  boolTypeName <- newName "bool"
  unionTypeName <- newName "union"
  ((bndrs, preds), augmentedTyp) <- augmentFinalType unionTypeName boolTypeName ty1
  return $ ForallT (bndrs ++ tybinders) (preds ++ ctx) augmentedTyp
augmentNormalCType t = do
  boolTypeName <- newName "bool"
  unionTypeName <- newName "union"
  ((bndrs, preds), augmentedTyp) <- augmentFinalType unionTypeName boolTypeName t
  return $ ForallT bndrs preds augmentedTyp

mkSingleWrapper :: String -> Con -> Q [Dec]
mkSingleWrapper name (NormalC oriName b) = do
  DataConI _ constructorTyp _ <- reify oriName
  augmentedTyp <- augmentNormalCType constructorTyp
  let retName = mkName name
  expr <- augmentNormalCExpr (length b) (ConE oriName)
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
mkSingleWrapper name (RecC oriName b) = do
  DataConI _ constructorTyp _ <- reify oriName
  augmentedTyp <- augmentNormalCType constructorTyp
  let retName = mkName name
  expr <- augmentNormalCExpr (length b) (ConE oriName)
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
mkSingleWrapper _ v = fail $ "Unsupported constructor" ++ pprint v
