{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.FunInstanceGen
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.FunInstanceGen
  ( supportedPrimFun,
    supportedPrimFunUpTo,
  )
where

import qualified Data.SBV as SBV
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind,
    SupportedNonFuncPrim,
    SupportedPrim
      ( castTypedSymbol,
        conSBVTerm,
        defaultValue,
        funcDummyConstraint,
        isFuncType,
        parseSMTModelResult,
        pevalDistinctTerm,
        pevalEqTerm,
        pevalITETerm,
        sbvDistinct,
        sbvEq,
        symSBVName,
        symSBVTerm,
        withPrim
      ),
    TypedSymbol (TypedSymbol),
    decideSymbolKind,
    translateTypeError,
    withNonFuncPrim,
  )
import Language.Haskell.TH
  ( Cxt,
    Dec (InstanceD),
    DecsQ,
    Exp,
    ExpQ,
    Name,
    Overlap (Overlapping),
    Q,
    Type,
    TypeQ,
    forallT,
    lamE,
    newName,
    sigD,
    stringE,
    varE,
    varP,
    varT,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( plainTVInferred,
    plainTVSpecified,
  )
import Type.Reflection (TypeRep, typeRep, type (:~~:) (HRefl))

instanceWithOverlapDescD ::
  Maybe Overlap -> Q Cxt -> Q Type -> [DecsQ] -> DecsQ
instanceWithOverlapDescD o ctxts ty descs = do
  ctxts1 <- ctxts
  descs1 <- sequence descs
  ty1 <- ty
  return [InstanceD o ctxts1 ty1 (concat descs1)]

-- | Generate an instance of 'SupportedPrim' for a function with a given number
-- of arguments.
supportedPrimFun ::
  ExpQ ->
  ExpQ ->
  ExpQ ->
  ([TypeQ] -> ExpQ) ->
  String ->
  String ->
  Name ->
  Int ->
  DecsQ
supportedPrimFun
  dv
  ite
  parse
  consbv
  funNameInError
  funNamePrefix
  funTypeName
  numArg = do
    names <- traverse (newName . ("a" <>) . show) [0 .. numArg - 1]

    let tyVars = varT <$> names
    knd <- newName "knd"
    knd' <- newName "knd'"
    let kndty = varT knd
    let knd'ty = varT knd'
    instanceWithOverlapDescD
      (if numArg == 2 then Nothing else Just Overlapping)
      (constraints tyVars)
      [t|SupportedPrim $(funType tyVars)|]
      ( [ [d|$(varP 'defaultValue) = $dv|],
          [d|$(varP 'pevalITETerm) = $ite|],
          [d|
            $(varP 'pevalEqTerm) =
              $( translateError
                   tyVars
                   "does not supported equality comparison."
               )
            |],
          [d|
            $(varP 'pevalDistinctTerm) =
              $( translateError
                   tyVars
                   "does not supported equality comparison."
               )
            |],
          [d|
            $(varP 'conSBVTerm) = $(consbv tyVars)
            |],
          -- \$( translateError
          --      tyVars
          --      ( "must have already been partially evaluated away before "
          --          <> "reaching this point."
          --      )
          --  )

          [d|
            $(varP 'symSBVName) = \_ num ->
              $(stringE $ funNamePrefix <> show numArg <> "_") <> show num
            |],
          [d|
            $(varP 'symSBVTerm) = \r ->
              withPrim @($(funType tyVars)) $ return $ SBV.uninterpret r
            |],
          [d|$(varP 'withPrim) = $(withPrims tyVars)|],
          [d|
            $(varP 'sbvEq) =
              $( translateError
                   tyVars
                   "does not support equality comparison."
               )
            |],
          [d|
            $(varP 'sbvDistinct) =
              $( translateError
                   tyVars
                   "does not support equality comparison."
               )
            |],
          [d|$(varP 'parseSMTModelResult) = $parse|],
          (: [])
            <$> sigD
              'castTypedSymbol
              ( forallT
                  [plainTVInferred knd, plainTVSpecified knd']
                  ((: []) <$> [t|IsSymbolKind $knd'ty|])
                  [t|
                    TypedSymbol $kndty $(funType tyVars) ->
                    Maybe (TypedSymbol $knd'ty $(funType tyVars))
                    |]
              ),
          [d|
            $(varP 'castTypedSymbol) = \(TypedSymbol sym) ->
              case decideSymbolKind @($knd'ty) of
                Left HRefl -> Nothing
                Right HRefl -> Just $ TypedSymbol sym
            |],
          [d|$(varP 'isFuncType) = True|],
          ( if numArg == 2
              then
                [d|
                  $(varP 'funcDummyConstraint) = \f ->
                    withPrim @($(funType tyVars)) $
                      withNonFuncPrim @($(last tyVars)) $ do
                        f (conSBVTerm (defaultValue :: $(head tyVars)))
                          SBV..== f
                            (conSBVTerm (defaultValue :: $(head tyVars)))
                  |]
              else
                [d|
                  $(varP 'funcDummyConstraint) = \f ->
                    withNonFuncPrim @($(head tyVars)) $
                      funcDummyConstraint @($(funType $ tail tyVars))
                        (f (conSBVTerm (defaultValue :: $(head tyVars))))
                  |]
          )
        ]
      )
    where
      translateError tyVars finalMsg =
        [|
          translateTypeError
            ( Just
                $( stringE $
                     "BUG. Please send a bug report. "
                       <> funNameInError
                       <> " "
                       <> finalMsg
                 )
            )
            (typeRep :: TypeRep $(funType tyVars))
          |]

      constraints = traverse (\ty -> [t|SupportedNonFuncPrim $ty|])
      funType =
        foldl1 (\fty ty -> [t|$(varT funTypeName) $ty $fty|]) . reverse
      withPrims :: [Q Type] -> Q Exp
      withPrims tyVars = do
        r <- newName "r"
        lamE [varP r] $
          foldr
            (\ty r -> [|withNonFuncPrim @($ty) $r|])
            (varE r)
            tyVars

-- | Generate instances of 'SupportedPrim' for functions with up to a given
-- number of arguments.
supportedPrimFunUpTo ::
  ExpQ -> ExpQ -> ExpQ -> ([TypeQ] -> ExpQ) -> String -> String -> Name -> Int -> DecsQ
supportedPrimFunUpTo
  dv
  ite
  parse
  consbv
  funNameInError
  funNamePrefix
  funTypeName
  numArg =
    concat
      <$> sequence
        [ supportedPrimFun
            dv
            ite
            parse
            consbv
            funNameInError
            funNamePrefix
            funTypeName
            n
          | n <- [2 .. numArg]
        ]
