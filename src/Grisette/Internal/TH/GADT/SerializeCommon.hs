{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.SerializeCommon
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.SerializeCommon
  ( serializeConfig,
    serializeWithSerialConfig,
  )
where

import Control.Monad (zipWithM)
import Data.Bytes.Serial (Serial (deserialize, serialize), Serial1, Serial2)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import GHC.Word (Word16, Word32, Word64, Word8)
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
        unaryOpConfigs,
        unaryOpContextNames,
        unaryOpExtraVars,
        unaryOpInstanceNames,
        unaryOpInstanceTypeFromConfig
      ),
    UnaryOpConfig (UnaryOpConfig),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraLiftedPatNames,
        extraPatNames,
        fieldCombineFun,
        fieldFunExp,
        fieldResFun
      ),
    UnaryOpFunConfig (genUnaryOpFun),
    defaultFieldFunExp,
    defaultUnaryOpInstanceTypeFromConfig,
  )
import Grisette.Internal.TH.Util (integerE)
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD),
    Lit (IntegerL),
    Match (Match),
    Name,
    Pat (LitP, VarP, WildP),
    Type (VarT),
    bindS,
    caseE,
    clause,
    conE,
    conT,
    doE,
    funD,
    match,
    mkName,
    newName,
    noBindS,
    normalB,
    sigP,
    varE,
    varP,
    wildP,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields, constructorName),
    TypeSubstitution (freeVariables),
    resolveTypeSynonyms,
  )

data UnaryOpSerializeWithSerialConfig = UnaryOpSerializeWithSerialConfig

instance UnaryOpFunConfig UnaryOpSerializeWithSerialConfig where
  genUnaryOpFun _ UnaryOpSerializeWithSerialConfig funNames n _ _ _ _ _ =
    funD (funNames !! n) [clause [] (normalB [|serialize|]) []]

data UnaryOpDeserializeWithSerialConfig = UnaryOpDeserializeWithSerialConfig

instance UnaryOpFunConfig UnaryOpDeserializeWithSerialConfig where
  genUnaryOpFun _ UnaryOpDeserializeWithSerialConfig funNames n _ _ _ _ _ =
    funD (funNames !! n) [clause [] (normalB [|deserialize|]) []]

-- | Configuration for deserialization function, generate the function from
-- scratch.
data UnaryOpDeserializeConfig = UnaryOpDeserializeConfig

getSerializedType :: Int -> Name
getSerializedType numConstructors =
  if
    | numConstructors <= fromIntegral (maxBound @Word8) + 1 -> ''Word8
    | numConstructors <= fromIntegral (maxBound @Word16) + 1 -> ''Word16
    | numConstructors <= fromIntegral (maxBound @Word32) + 1 -> ''Word32
    | numConstructors <= fromIntegral (maxBound @Word64) + 1 -> ''Word64
    | otherwise -> ''Integer

instance UnaryOpFunConfig UnaryOpDeserializeConfig where
  genUnaryOpFun
    _
    UnaryOpDeserializeConfig
    funNames
    n
    _
    _
    argTypes
    _
    constructors = do
      allFields <-
        mapM resolveTypeSynonyms $
          concatMap constructorFields constructors
      let usedArgs = S.fromList $ freeVariables allFields
      args <-
        traverse
          ( \(ty, _) -> do
              case ty of
                VarT nm ->
                  if S.member nm usedArgs
                    then do
                      pname <- newName "p"
                      return (nm, Just pname)
                    else return ('undefined, Nothing)
                _ -> return ('undefined, Nothing)
          )
          argTypes
      let argToFunPat =
            M.fromList $ mapMaybe (\(nm, mpat) -> fmap (nm,) mpat) args
      let funPats = fmap (maybe WildP VarP . snd) args
      let genAuxFunMatch conIdx conInfo = do
            fields <- mapM resolveTypeSynonyms $ constructorFields conInfo
            defaultFieldFunExps <-
              traverse
                ( defaultFieldFunExp
                    funNames
                    argToFunPat
                    M.empty
                )
                fields
            let conName = constructorName conInfo
            exp <-
              foldl
                (\exp fieldFun -> [|$exp <*> $(return fieldFun)|])
                [|return $(conE conName)|]
                defaultFieldFunExps
            return $ Match (LitP (IntegerL conIdx)) (NormalB exp) []
      auxMatches <- zipWithM genAuxFunMatch [0 ..] constructors
      auxFallbackMatch <- match wildP (normalB [|undefined|]) []
      let instanceFunName = funNames !! n
      -- let auxFunName = mkName "go"
      let selName = mkName "sel"
      exp <-
        doE
          [ bindS
              ( sigP
                  (varP selName)
                  (conT (getSerializedType $ length constructors))
              )
              (varE (head funNames)),
            noBindS $
              caseE (varE selName) $
                return <$> auxMatches ++ [auxFallbackMatch]
          ]
      return $
        FunD
          instanceFunName
          [ Clause
              funPats
              (NormalB exp)
              []
          ]

-- | Configuration for serialization function, generate the function from
-- scratch.
serializeConfig :: [Name] -> [Name] -> [Name] -> UnaryOpClassConfig
serializeConfig instanceNames serializeFunNames deserializeFunNames =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = [],
                extraLiftedPatNames = const [],
                fieldCombineFun = \totalConNumber conIdx _ _ [] exp -> do
                  let ty = getSerializedType totalConNumber
                  r <-
                    foldl
                      (\r exp -> [|$r >> $(return exp)|])
                      ( [|
                          $(varE $ head serializeFunNames)
                            ($(integerE conIdx) :: $(conT ty))
                          |]
                      )
                      exp
                  return (r, [True]),
                fieldResFun = \_ _ _ _ fieldPat fieldFun -> do
                  r <- [|$(return fieldFun) $(return fieldPat)|]
                  return (r, [True]),
                fieldFunExp = defaultFieldFunExp serializeFunNames
              }
            serializeFunNames,
          UnaryOpConfig
            UnaryOpDeserializeConfig
            deserializeFunNames
        ],
      unaryOpInstanceNames = instanceNames,
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = False,
      unaryOpContextNames = Nothing
    }

-- | Configuration for serialization function, reuse the 'Serial' instance.
serializeWithSerialConfig :: [Name] -> [Name] -> [Name] -> UnaryOpClassConfig
serializeWithSerialConfig instanceNames serializeFunNames deserializeFunNames =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig UnaryOpSerializeWithSerialConfig serializeFunNames,
          UnaryOpConfig UnaryOpDeserializeWithSerialConfig deserializeFunNames
        ],
      unaryOpInstanceNames = instanceNames,
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = False,
      unaryOpContextNames =
        Just $ take (length instanceNames) [''Serial, ''Serial1, ''Serial2]
    }
