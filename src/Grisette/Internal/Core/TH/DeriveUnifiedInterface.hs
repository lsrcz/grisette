{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Internal.Core.TH.DeriveUnifiedInterface
  ( deriveUnifiedInterface,
    deriveUnifiedInterfaces,
  )
where

import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode)
import Grisette.Unified.Internal.Util (withMode)
import Language.Haskell.TH
  ( Dec,
    Exp,
    Inline (Inline),
    Name,
    Phases (AllPhases),
    Q,
    RuleMatch (FunLike),
    Type (ConT, StarT, VarT),
    conT,
    instanceD,
    kindedTV,
    lam1E,
    normalB,
    pprint,
    pragInlD,
    valD,
    varE,
    varP,
    varT,
  )
import Language.Haskell.TH.Datatype
  ( DatatypeInfo (datatypeVars),
    datatypeType,
    reifyDatatype,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndr_,
    tvKind,
    tvName,
  )
import Language.Haskell.TH.Syntax (newName)

tvIsMode :: TyVarBndr_ flag -> Bool
tvIsMode = (== ConT ''EvaluationMode) . tvKind

tvIsNat :: TyVarBndr_ flag -> Bool
tvIsNat = (== ConT ''Nat) . tvKind

tvIsStar :: TyVarBndr_ flag -> Bool
tvIsStar = (== StarT) . tvKind

tvIsUnsupported :: TyVarBndr_ flag -> Bool
tvIsUnsupported bndr = not $ tvIsMode bndr || tvIsNat bndr || tvIsStar bndr

tvType :: TyVarBndr_ flag -> Type
tvType = VarT . tvName

data TyVarCategorized flag = TyVarCategorized
  { modeTyVars :: [Q Type],
    natTyVars :: [Q Type],
    starTyVars :: [Q Type],
    unsupportedTyVars :: [Q Type]
  }

categorizeTyVars :: [TyVarBndr_ flag] -> TyVarCategorized flag
categorizeTyVars = foldr categorize (TyVarCategorized [] [] [] [])
  where
    categorize bndr acc
      | tvIsMode bndr = acc {modeTyVars = return (tvType bndr) : modeTyVars acc}
      | tvIsNat bndr = acc {natTyVars = return (tvType bndr) : natTyVars acc}
      | tvIsStar bndr = acc {starTyVars = return (tvType bndr) : starTyVars acc}
      | tvIsUnsupported bndr =
          acc {unsupportedTyVars = return (tvType bndr) : unsupportedTyVars acc}
      | otherwise = acc

deriveUnifiedInterface :: Name -> Name -> Name -> Q [Dec]
deriveUnifiedInterface cls withFunc name = do
  d <- reifyDatatype name
  let tyVars = datatypeVars d
  let categorizedTyVars = categorizeTyVars tyVars
  (mode, allTyVars) <- case modeTyVars categorizedTyVars of
    [] -> do
      nm <- newName "mode"
      let var = varT nm
      return (var, kindedTV nm (ConT ''EvaluationMode) : tyVars)
    [var] -> return (var, tyVars)
    _ ->
      fail "The number of mode type arguments must be less than or equal to 1."
  bndrConstraints <-
    concat <$> mapM (genBndrConstraint mode) allTyVars
  sequence
    [ instanceD
        (return bndrConstraints)
        [t|$(conT cls) $mode $(return $ datatypeType d)|]
        [ body mode $ starTyVars categorizedTyVars,
          pragInlD withFunc Inline FunLike AllPhases
        ]
    ]
  where
    genBndrConstraint :: Q Type -> TyVarBndr_ flag -> Q [Type]
    genBndrConstraint mode bndr = do
      let name = tvName bndr
      let tv = return $ VarT name
      let kind = tvKind bndr
      case kind of
        StarT -> sequence [[t|$(conT cls) $mode $tv|], [t|Mergeable $tv|]]
        (ConT nm) | nm == ''EvaluationMode -> (: []) <$> [t|Typeable $tv|]
        (ConT nm)
          | nm == ''Nat -> sequence [[t|KnownNat $tv|], [t|1 <= $tv|]]
        _ -> fail $ "Unsupported kind in type arguments: " ++ pprint kind
    applyWithBaseSEq :: Q Type -> Q Type -> Q Exp -> Q Exp
    applyWithBaseSEq mode var exp = [|$(varE withFunc) @($mode) @($var) $exp|]
    body :: Q Type -> [Q Type] -> Q Dec
    body mode starVars = do
      var <- newName "r"
      let arg = varP var
      let branch = foldr (applyWithBaseSEq mode) (varE var) starVars
      let exp = lam1E arg [|withMode @($mode) $branch $branch|]
      valD (varP withFunc) (normalB exp) []

deriveUnifiedInterfaces :: Name -> Name -> [Name] -> Q [Dec]
deriveUnifiedInterfaces cls withFunc names =
  concat <$> mapM (deriveUnifiedInterface cls withFunc) names
