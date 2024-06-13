module Grisette.Internal.Core.TH.Util (occName, constructorInfoToType) where

import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorContext, constructorFields, constructorVars),
    DatatypeInfo (datatypeVars),
    datatypeType,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( Specificity (SpecifiedSpec),
    mapTVFlag,
  )
import Language.Haskell.TH.Syntax
  ( Name (Name),
    OccName (OccName),
    Q,
    Type (AppT, ArrowT, ForallT),
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
