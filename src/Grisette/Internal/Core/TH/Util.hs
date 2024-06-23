{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.Core.TH.Util
  ( occName,
    constructorInfoToType,
    tvIsMode,
    tvIsNat,
    tvIsStar,
    tvIsUnsupported,
    tvType,
    TyVarCategorized (..),
    categorizeTyVars,
    tvIsStarToStar,
  )
where

import GHC.TypeNats (Nat)
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode)
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorContext, constructorFields, constructorVars),
    DatatypeInfo (datatypeVars),
    datatypeType,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( Specificity (SpecifiedSpec),
    TyVarBndr_,
    mapTVFlag,
    tvKind,
    tvName,
  )
import Language.Haskell.TH.Syntax
  ( Name (Name),
    OccName (OccName),
    Q,
    Type (AppT, ArrowT, ConT, ForallT, StarT, VarT),
  )

occName :: Name -> String
occName (Name (OccName name) _) = name

constructorInfoToType :: DatatypeInfo -> ConstructorInfo -> Q Type
constructorInfoToType dataType info = do
  let binders =
        mapTVFlag (const SpecifiedSpec)
          <$> datatypeVars dataType ++ constructorVars info
  let ctx = constructorContext info
  let fields = constructorFields info
  let tyBody =
        foldr (AppT . AppT ArrowT) (datatypeType dataType) fields
  if null binders then return tyBody else return $ ForallT binders ctx tyBody

tvIsMode :: TyVarBndr_ flag -> Bool
tvIsMode = (== ConT ''EvaluationMode) . tvKind

tvIsNat :: TyVarBndr_ flag -> Bool
tvIsNat = (== ConT ''Nat) . tvKind

tvIsStar :: TyVarBndr_ flag -> Bool
tvIsStar = (== StarT) . tvKind

tvIsStarToStar :: TyVarBndr_ flag -> Bool
tvIsStarToStar = (== (AppT (AppT ArrowT StarT) StarT)) . tvKind

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
