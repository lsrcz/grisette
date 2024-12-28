{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Internal.TH.GADT.DeriveSerial
  ( deriveGADTSerial,
    deriveGADTSerial1,
    deriveGADTSerial2,
  )
where

import Control.Monad (zipWithM)
import Data.Bytes.Serial
  ( Serial (deserialize, serialize),
    Serial1 (deserializeWith, serializeWith),
    Serial2 (deserializeWith2, serializeWith2),
  )
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Grisette.Internal.TH.GADT.Common (DeriveConfig)
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( FieldFunExp,
    UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
        unaryOpConfigs,
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
    genUnaryOpClass,
  )
import Grisette.Internal.TH.Util (integerE)
import Language.Haskell.TH (Body (NormalB), Clause (Clause), Dec (FunD), Lit (IntegerL), Match (Match), Name, Pat (LitP, VarP, WildP), Q, Quote (newName), Type (VarT), bindS, caseE, conE, conT, doE, match, mkName, noBindS, normalB, sigP, varE, varP, wildP)
import Language.Haskell.TH.Datatype (ConstructorInfo (constructorFields, constructorName), TypeSubstitution (freeVariables), resolveTypeSynonyms)

newtype UnaryOpDeserializeConfig = UnaryOpDeserializeConfig
  {fieldDeserializeFun :: FieldFunExp}

instance UnaryOpFunConfig UnaryOpDeserializeConfig where
  genUnaryOpFun
    _
    UnaryOpDeserializeConfig {..}
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
                (fieldDeserializeFun argToFunPat M.empty)
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
              (sigP (varP selName) (conT ''Int))
              [|deserialize|],
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

serialConfig :: UnaryOpClassConfig
serialConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = [],
                extraLiftedPatNames = const [],
                fieldCombineFun = \conIdx _ _ [] exp -> do
                  r <-
                    foldl
                      (\r exp -> [|$r >> $(return exp)|])
                      ([|serialize ($(integerE conIdx) :: Int)|])
                      exp
                  return (r, [True]),
                fieldResFun = \_ _ _ _ fieldPat fieldFun -> do
                  r <- [|$(return fieldFun) $(return fieldPat)|]
                  return (r, [True]),
                fieldFunExp =
                  defaultFieldFunExp
                    ['serialize, 'serializeWith, 'serializeWith2]
              }
            ['serialize, 'serializeWith, 'serializeWith2],
          UnaryOpConfig
            UnaryOpDeserializeConfig
              { fieldDeserializeFun =
                  defaultFieldFunExp
                    ['deserialize, 'deserializeWith, 'deserializeWith2]
              }
            ['deserialize, 'deserializeWith, 'deserializeWith2]
        ],
      unaryOpInstanceNames = [''Serial, ''Serial1, ''Serial2],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = False
    }

-- | Derive 'Serial' instance for a GADT.
deriveGADTSerial :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial deriveConfig = genUnaryOpClass deriveConfig serialConfig 0

-- | Derive 'Serial1' instance for a GADT.
deriveGADTSerial1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial1 deriveConfig = genUnaryOpClass deriveConfig serialConfig 1

-- | Derive 'Serial2' instance for a GADT.
deriveGADTSerial2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSerial2 deriveConfig = genUnaryOpClass deriveConfig serialConfig 2
