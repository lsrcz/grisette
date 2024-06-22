module Grisette.Internal.TH.Util (substDataType, reifyDatatypeWithFreshNames) where

import qualified Data.Map as M
import Language.Haskell.TH (Name, Q, Type (VarT), newName)
import Language.Haskell.TH.Datatype
  ( DatatypeInfo (datatypeCons, datatypeInstTypes, datatypeVars),
    TypeSubstitution (applySubstitution),
    reifyDatatype,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (mapTVName)

substDataType :: DatatypeInfo -> M.Map Name Type -> DatatypeInfo
substDataType d substMap =
  d
    { datatypeInstTypes = applySubstitution substMap <$> datatypeInstTypes d,
      datatypeCons = applySubstitution substMap <$> datatypeCons d
    }

datatypeToFreshNames :: DatatypeInfo -> Q DatatypeInfo
datatypeToFreshNames d = do
  let vars = datatypeVars d
  let names = tvName <$> vars
  freshNames <- traverse (newName . show) names
  let newDTVars = zipWith (\v n -> mapTVName (const n) v) vars freshNames
  let substMap = M.fromList $ zip names (VarT <$> freshNames)
  return $ substDataType d {datatypeVars = newDTVars} substMap

reifyDatatypeWithFreshNames :: Name -> Q DatatypeInfo
reifyDatatypeWithFreshNames name = do
  d <- reifyDatatype name
  datatypeToFreshNames d