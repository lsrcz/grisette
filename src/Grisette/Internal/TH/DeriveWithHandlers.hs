{-# LANGUAGE TupleSections #-}

module Grisette.Internal.TH.DeriveWithHandlers
  ( deriveWithHandlers,
  )
where

import Control.Monad (foldM, unless, when)
import Data.List (transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( DeriveInstanceProvider (instanceDeclaration),
  )
import Grisette.Internal.TH.DeriveTypeParamHandler
  ( DeriveTypeParamHandler (handleBody, handleTypeParams),
    SomeDeriveTypeParamHandler,
  )
import Grisette.Internal.TH.Util
  ( allSameKind,
    dropNTypeParam,
    reifyDatatypeWithFreshNames,
    substDataType,
  )
import Language.Haskell.TH (Dec, Name, Q)
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields),
    DatatypeInfo (datatypeCons, datatypeVars),
    datatypeType,
    reifyDatatype,
    tvName,
  )

transposeMatrix :: Int -> [[a]] -> [[a]]
transposeMatrix n [] = replicate n []
transposeMatrix _ x = transpose x

deriveWithHandlers ::
  (DeriveInstanceProvider provider) =>
  [SomeDeriveTypeParamHandler] ->
  provider ->
  Bool ->
  Int ->
  [Name] ->
  Q [Dec]
deriveWithHandlers
  handlers
  provider
  ignoreBodyConstraints
  numDroppedTailTypes
  names = do
    when (numDroppedTailTypes < 0) $
      fail "deriveWithHandlers: numDroppedTailTypes must be non-negative"
    when (numDroppedTailTypes > 0 && not ignoreBodyConstraints) $
      fail $
        "deriveWithHandlers: ignoreBodyConstraints must be True if "
          <> "numDroppedTailTypes > 0"
    when (null names) $
      fail "deriveWithHandlers: no types provided"
    datatypes <-
      if length names == 1
        then mapM reifyDatatype names
        else mapM reifyDatatypeWithFreshNames names

    let tyVars =
          transposeMatrix 0 $
            map
              (reverse . drop numDroppedTailTypes . reverse . datatypeVars)
              datatypes
    unless (all allSameKind tyVars) $
      fail "deriveWithHandlers: all type variables must be aligned"
    tyVarsWithConstraints <-
      foldM
        (flip $ handleTypeParams (length datatypes))
        ( map
            (\tyVarList -> ((,Nothing) <$> tyVarList, Nothing))
            tyVars
        )
        handlers

    let allTyVarsConstraints =
          concatMap (fromMaybe [] . snd) tyVarsWithConstraints
    let tvWithSubst =
          transposeMatrix (length datatypes) $
            fst <$> tyVarsWithConstraints

    let substMaps =
          map
            ( M.fromList
                . mapMaybe
                  ( \(tv, t) -> do
                      substTy <- t
                      return (tvName tv, substTy)
                  )
            )
            tvWithSubst
    let substedTypes = zipWith substDataType datatypes substMaps
    tys <-
      mapM (dropNTypeParam numDroppedTailTypes . datatypeType) substedTypes
    allConstraints <-
      ( if ignoreBodyConstraints
          then return allTyVarsConstraints
          else do
            bodyConstraints <- handleBodyWithHandlers substedTypes handlers
            return $ allTyVarsConstraints ++ bodyConstraints
        )
    instanceDeclaration
      provider
      (fst <$> tyVarsWithConstraints)
      allConstraints
      tys
    where
      handleBodyWithHandlers datatypes handlers = do
        let cons = datatypeCons <$> datatypes
        let zippedFields = zipFields cons
        concat <$> traverse (`handleBody` zippedFields) handlers
      zipFields cons = transpose $ concatMap constructorFields <$> cons
