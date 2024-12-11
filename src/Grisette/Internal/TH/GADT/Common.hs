module Grisette.Internal.TH.GADT.Common
  ( checkArgs,
  )
where

import Control.Monad (when)
import qualified Data.Map as M
import Grisette.Internal.TH.Util (occName)
import Language.Haskell.TH
  ( Name,
    Q,
    Type (VarT),
    newName,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo,
    DatatypeInfo (datatypeCons, datatypeVars),
    TypeSubstitution (applySubstitution),
    reifyDatatype,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndr_, mapTVName)

checkArgs ::
  String ->
  Int ->
  Name ->
  Int ->
  Q
    ( [ConstructorInfo],
      [Name],
      [TyVarBndr_ ()],
      [Name],
      [TyVarBndr_ ()]
    )
checkArgs clsName maxArgNum typName n = do
  when (n < 0) $
    fail $
      unlines
        [ "Cannot derive "
            ++ clsName
            ++ " instance with negative type parameters",
          "Requested: " ++ show n,
          "Hint: Use a non-negative number of type parameters"
        ]
  when (n > maxArgNum) $
    fail $
      "Requesting "
        <> clsName
        <> " instance with more than "
        <> show maxArgNum
        <> " type parameters"
  d <- reifyDatatype typName
  let dvars = datatypeVars d
  when (length dvars < n) $
    fail $
      "Requesting Mergeable"
        <> show n
        <> " instance, while the type "
        <> show typName
        <> " has only "
        <> show (length dvars)
        <> " type variables."
  let keptVars = take (length dvars - n) dvars
  keptNewNames <- traverse (newName . occName . tvName) keptVars
  let keptNewVars =
        zipWith (mapTVName . const) keptNewNames keptVars
  let argVars = drop (length dvars - n) dvars
  argNewNames <- traverse (newName . occName . tvName) argVars
  let argNewVars =
        zipWith (mapTVName . const) argNewNames argVars
  let substMap =
        M.fromList $
          zip
            (tvName <$> dvars)
            (VarT <$> keptNewNames ++ argNewNames)
  let constructors = datatypeCons d
  return
    ( applySubstitution substMap constructors,
      keptNewNames,
      keptNewVars,
      argNewNames,
      argNewVars
    )
