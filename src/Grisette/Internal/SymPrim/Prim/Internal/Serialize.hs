{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Internal.SymPrim.Prim.Internal.Serialize () where

import Control.Monad (replicateM, unless, when)
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State
import qualified Data.Binary as Binary
import Data.Bytes.Get (MonadGet (getWord8))
import Data.Bytes.Put (MonadPut (putWord8))
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Serialize as Cereal
import Data.Word (Word8)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    ValidFP,
    checkDynamicValidFP,
    invalidFPMessage,
    withUnsafeValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Caches (Id)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitwiseTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalOrdTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalRotateTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalShiftTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind (decideSymbolKind),
    PEvalBitwiseTerm,
    PEvalNumTerm,
    PEvalOrdTerm,
    PEvalRotateTerm,
    PEvalShiftTerm,
    SomeTypedAnySymbol,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedNonFuncPrim,
    SupportedPrim (primTypeRep, withPrim),
    Term
      ( AbsNumTerm,
        AddNumTerm,
        AndBitsTerm,
        AndTerm,
        ComplementBitsTerm,
        ConTerm,
        EqTerm,
        ExistsTerm,
        ForallTerm,
        ITETerm,
        LeOrdTerm,
        LtOrdTerm,
        MulNumTerm,
        NegNumTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        RotateLeftTerm,
        RotateRightTerm,
        ShiftLeftTerm,
        ShiftRightTerm,
        SignumNumTerm,
        SymTerm,
        XorBitsTerm
      ),
    TypedAnySymbol,
    TypedConstantSymbol,
    TypedSymbol (TypedSymbol),
    absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    complementBitsTerm,
    conTerm,
    distinctTerm,
    eqTerm,
    existsTerm,
    forallTerm,
    introSupportedPrimConstraint,
    iteTerm,
    leOrdTerm,
    ltOrdTerm,
    mulNumTerm,
    negNumTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    someTypedSymbol,
    symTerm,
    termId,
    withSupportedPrimTypeable,
    xorBitsTerm,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
    someTerm,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils (castTerm)
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.Utils.Parameterized
  ( NatRepr,
    SomePositiveNatRepr (SomePositiveNatRepr),
    mkPositiveNatRepr,
  )
import Type.Reflection
  ( SomeTypeRep (SomeTypeRep),
    TypeRep,
    Typeable,
    eqTypeRep,
    someTypeRep,
    typeRep,
    pattern App,
    pattern Con,
    type (:~~:) (HRefl),
  )

data KnownNonFuncType where
  BoolType :: KnownNonFuncType
  IntegerType :: KnownNonFuncType
  WordNType :: (KnownNat n, 1 <= n) => Proxy n -> KnownNonFuncType
  IntNType :: (KnownNat n, 1 <= n) => Proxy n -> KnownNonFuncType
  FPType :: (ValidFP eb sb) => Proxy eb -> Proxy sb -> KnownNonFuncType
  FPRoundingModeType :: KnownNonFuncType
  AlgRealType :: KnownNonFuncType

data KnownNonFuncTypeWitness where
  KnownNonFuncTypeWitness ::
    ( SupportedNonFuncPrim a,
      Eq a,
      Show a,
      Hashable a,
      Typeable a,
      Serial a
    ) =>
    Proxy a ->
    KnownNonFuncTypeWitness

witnessKnownNonFuncType :: KnownNonFuncType -> KnownNonFuncTypeWitness
witnessKnownNonFuncType BoolType = KnownNonFuncTypeWitness (Proxy @Bool)
witnessKnownNonFuncType IntegerType = KnownNonFuncTypeWitness (Proxy @Integer)
witnessKnownNonFuncType (WordNType (Proxy :: Proxy n)) =
  KnownNonFuncTypeWitness (Proxy @(WordN n))
witnessKnownNonFuncType (IntNType (Proxy :: Proxy n)) =
  KnownNonFuncTypeWitness (Proxy @(IntN n))
witnessKnownNonFuncType (FPType (Proxy :: Proxy eb) (Proxy :: Proxy sb)) =
  KnownNonFuncTypeWitness (Proxy @(FP eb sb))
witnessKnownNonFuncType FPRoundingModeType =
  KnownNonFuncTypeWitness (Proxy @FPRoundingMode)
witnessKnownNonFuncType AlgRealType = KnownNonFuncTypeWitness (Proxy @AlgReal)

data KnownType where
  NonFuncType :: KnownNonFuncType -> KnownType
  TabularFunType :: [KnownNonFuncType] -> KnownType
  GeneralFunType :: [KnownNonFuncType] -> KnownType

data KnownTypeWitness where
  KnownTypeWitness ::
    ( SupportedPrim a,
      Eq a,
      Show a,
      Hashable a,
      Typeable a,
      Serial a
    ) =>
    Proxy a ->
    KnownTypeWitness

witnessKnownType :: KnownType -> KnownTypeWitness
witnessKnownType (NonFuncType nf) = case witnessKnownNonFuncType nf of
  KnownNonFuncTypeWitness (Proxy :: Proxy a) -> KnownTypeWitness (Proxy @a)
witnessKnownType (TabularFunType [a, b]) =
  case (witnessKnownNonFuncType a, witnessKnownNonFuncType b) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b)
      ) -> KnownTypeWitness (Proxy @(a =-> b))
witnessKnownType (TabularFunType [a, b, c]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c))
witnessKnownType (TabularFunType [a, b, c, d]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d))
witnessKnownType (TabularFunType [a, b, c, d, e]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e))
witnessKnownType (TabularFunType [a, b, c, d, e, f]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e =-> f))
witnessKnownType (TabularFunType [a, b, c, d, e, f, g]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e =-> f =-> g))
witnessKnownType (TabularFunType [a, b, c, d, e, f, g, h]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g,
         witnessKnownNonFuncType h
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g),
      KnownNonFuncTypeWitness (Proxy :: Proxy h)
      ) ->
        KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e =-> f =-> g =-> h))
witnessKnownType (GeneralFunType [a, b]) =
  case (witnessKnownNonFuncType a, witnessKnownNonFuncType b) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b)
      ) -> KnownTypeWitness (Proxy @(a --> b))
witnessKnownType (GeneralFunType [a, b, c]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c))
witnessKnownType (GeneralFunType [a, b, c, d]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d))
witnessKnownType (GeneralFunType [a, b, c, d, e]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d --> e))
witnessKnownType (GeneralFunType [a, b, c, d, e, f]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d --> e --> f))
witnessKnownType (GeneralFunType [a, b, c, d, e, f, g]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d --> e --> f --> g))
witnessKnownType (GeneralFunType [a, b, c, d, e, f, g, h]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g,
         witnessKnownNonFuncType h
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g),
      KnownNonFuncTypeWitness (Proxy :: Proxy h)
      ) ->
        KnownTypeWitness (Proxy @(a --> b --> c --> d --> e --> f --> g --> h))
witnessKnownType l = error $ "witnessKnownType: unsupported type: " <> show l

instance Show KnownNonFuncType where
  show BoolType = "Bool"
  show IntegerType = "Integer"
  show (WordNType (_ :: p n)) = "WordN " <> show (natVal (Proxy @n))
  show (IntNType (_ :: p n)) = "IntN " <> show (natVal (Proxy @n))
  show (FPType (_ :: p eb) (_ :: q sb)) =
    "FP "
      <> show (natVal (Proxy @eb))
      <> " "
      <> show (natVal (Proxy @sb))
  show FPRoundingModeType = "FPRoundingMode"
  show AlgRealType = "AlgReal"

instance Show KnownType where
  show (NonFuncType t) = show t
  show (TabularFunType ts) = intercalate " =-> " $ show <$> ts
  show (GeneralFunType ts) = intercalate " --> " $ show <$> ts

knownNonFuncType ::
  forall a p. (SupportedNonFuncPrim a) => p a -> KnownNonFuncType
knownNonFuncType _ =
  case tr of
    _ | SomeTypeRep tr == someTypeRep (Proxy @Bool) -> BoolType
    _ | SomeTypeRep tr == someTypeRep (Proxy @Integer) -> IntegerType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @FPRoundingMode) ->
          FPRoundingModeType
    _ | SomeTypeRep tr == someTypeRep (Proxy @AlgReal) -> AlgRealType
    App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
      case ( eqTypeRep ta (typeRep @WordN),
             eqTypeRep ta (typeRep @IntN)
           ) of
        (Just HRefl, _) -> withPrim @a $ WordNType (Proxy @n)
        (_, Just HRefl) -> withPrim @a $ IntNType (Proxy @n)
        _ -> err
    App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
      case eqTypeRep tf (typeRep @FP) of
        Just HRefl -> withPrim @a $ FPType (Proxy @a0) (Proxy @a1)
        _ -> err
    _ -> err
  where
    tr = primTypeRep @a
    err = error $ "knownNonFuncType: unsupported type: " <> show tr

knownType ::
  forall a p. (SupportedPrim a) => p a -> KnownType
knownType _ =
  case tr of
    _ | SomeTypeRep tr == someTypeRep (Proxy @Bool) -> NonFuncType BoolType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @Integer) ->
          NonFuncType IntegerType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @FPRoundingMode) ->
          NonFuncType FPRoundingModeType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @AlgReal) ->
          NonFuncType AlgRealType
    App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
      case ( eqTypeRep ta (typeRep @WordN),
             eqTypeRep ta (typeRep @IntN)
           ) of
        (Just HRefl, _) -> withPrim @a $ NonFuncType $ WordNType (Proxy @n)
        (_, Just HRefl) -> withPrim @a $ NonFuncType $ IntNType (Proxy @n)
        _ -> err
    App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
      case ( eqTypeRep tf (typeRep @FP),
             eqTypeRep tf (typeRep @(=->)),
             eqTypeRep tf (typeRep @(-->))
           ) of
        (Just HRefl, _, _) ->
          withPrim @a $ NonFuncType $ FPType (Proxy @a0) (Proxy @a1)
        (_, Just HRefl, _) ->
          withPrim @a $
            let arg = knownType (Proxy @a0)
                ret = knownType (Proxy @a1)
             in case arg of
                  NonFuncType n -> case ret of
                    NonFuncType m -> TabularFunType [n, m]
                    TabularFunType ns -> TabularFunType (n : ns)
                    _ -> err
                  _ -> err
        (_, _, Just HRefl) ->
          withPrim @a $
            let arg = knownType (Proxy @a0)
                ret = knownType (Proxy @a1)
             in case arg of
                  NonFuncType n -> case ret of
                    NonFuncType m -> GeneralFunType [n, m]
                    GeneralFunType ns -> GeneralFunType (n : ns)
                    _ -> err
                  _ -> err
        _ -> err
    _ -> err
  where
    tr = primTypeRep @a
    err = error $ "knownType: unsupported type: " <> show tr

-- Bool: 0
-- Integer: 1
-- WordN: 2
-- IntN: 3
-- FP: 4
-- FPRoundingMode: 5
-- AlgReal: 6
serializeKnownNonFuncType :: (MonadPut m) => KnownNonFuncType -> m ()
serializeKnownNonFuncType BoolType = putWord8 0
serializeKnownNonFuncType IntegerType = putWord8 1
serializeKnownNonFuncType (WordNType (Proxy :: Proxy n)) =
  putWord8 2 >> serialize (natVal (Proxy @n))
serializeKnownNonFuncType (IntNType (Proxy :: Proxy n)) =
  putWord8 3 >> serialize (natVal (Proxy @n))
serializeKnownNonFuncType (FPType (Proxy :: Proxy eb) (Proxy :: Proxy sb)) =
  putWord8 4 >> serialize (natVal (Proxy @eb)) >> serialize (natVal (Proxy @sb))
serializeKnownNonFuncType FPRoundingModeType = putWord8 5
serializeKnownNonFuncType AlgRealType = putWord8 6

serializeKnownType :: (MonadPut m) => KnownType -> m ()
serializeKnownType (NonFuncType t) = putWord8 0 >> serializeKnownNonFuncType t
serializeKnownType (TabularFunType ts) =
  putWord8 1
    >> putWord8 (fromIntegral $ length ts)
    >> traverse_ serializeKnownNonFuncType ts
serializeKnownType (GeneralFunType ts) =
  putWord8 2
    >> putWord8 (fromIntegral $ length ts)
    >> traverse_ serializeKnownNonFuncType ts

deserializeKnownNonFuncType :: (MonadGet m) => m KnownNonFuncType
deserializeKnownNonFuncType = do
  tag <- getWord8
  case tag of
    0 -> return BoolType
    1 -> return IntegerType
    2 -> do
      n <- deserialize @Natural
      when (n == 0) $ fail "WordN 0 is not allowed"
      case mkPositiveNatRepr n of
        SomePositiveNatRepr (_ :: NatRepr n) -> return $ WordNType (Proxy @n)
    3 -> do
      n <- deserialize @Natural
      when (n == 0) $ fail "IntN 0 is not allowed"
      case mkPositiveNatRepr n of
        SomePositiveNatRepr (_ :: NatRepr n) -> return $ IntNType (Proxy @n)
    4 -> do
      eb <- deserialize @Natural
      sb <- deserialize @Natural
      unless (checkDynamicValidFP eb sb) $ fail invalidFPMessage
      case (mkPositiveNatRepr eb, mkPositiveNatRepr sb) of
        ( SomePositiveNatRepr (_ :: NatRepr eb),
          SomePositiveNatRepr (_ :: NatRepr sb)
          ) ->
            withUnsafeValidFP @eb @sb $ return $ FPType (Proxy @eb) (Proxy @sb)
    5 -> return FPRoundingModeType
    6 -> return AlgRealType
    _ -> fail "Unknown type tag"

deserializeKnownType :: (MonadGet m) => m KnownType
deserializeKnownType = do
  tag <- getWord8
  case tag of
    0 -> NonFuncType <$> deserializeKnownNonFuncType
    1 -> do
      n <- getWord8
      nfs <- replicateM (fromIntegral n) deserializeKnownNonFuncType
      return $ TabularFunType nfs
    2 -> do
      n <- getWord8
      nfs <- replicateM (fromIntegral n) deserializeKnownNonFuncType
      return $ GeneralFunType nfs
    _ -> fail "Unknown type tag"

instance (IsSymbolKind knd) => Serial (SomeTypedSymbol knd) where
  serialize (SomeTypedSymbol tsb@(TypedSymbol sb)) =
    case decideSymbolKind @knd of
      Left HRefl -> do
        serializeKnownNonFuncType $ knownNonFuncType tsb
        serialize sb
      Right HRefl -> do
        serializeKnownType $ knownType tsb
        serialize sb
  deserialize = case decideSymbolKind @knd of
    Left HRefl -> do
      kt <- deserializeKnownNonFuncType
      case witnessKnownNonFuncType kt of
        KnownNonFuncTypeWitness (Proxy :: Proxy a) -> do
          sb <- deserialize
          return $ SomeTypedSymbol $ TypedSymbol @a sb
    Right HRefl -> do
      kt <- deserializeKnownType
      case witnessKnownType kt of
        KnownTypeWitness (Proxy :: Proxy a) -> do
          sb <- deserialize
          return $ SomeTypedSymbol $ TypedSymbol @a sb

instance (IsSymbolKind knd) => Cereal.Serialize (SomeTypedSymbol knd) where
  put = serialize
  get = deserialize

instance (IsSymbolKind knd) => Binary.Binary (SomeTypedSymbol knd) where
  put = serialize
  get = deserialize

instance (IsSymbolKind knd, Typeable a) => Serial (TypedSymbol knd a) where
  serialize tsb = serialize $ someTypedSymbol tsb
  deserialize = do
    SomeTypedSymbol (tsb@TypedSymbol {} :: TypedSymbol knd b) <- deserialize
    case eqTypeRep (typeRep @a) (primTypeRep @b) of
      Just HRefl -> return tsb
      Nothing -> fail "deserialize TypedSymbol: type mismatch"

instance
  (IsSymbolKind knd, Typeable a) =>
  Cereal.Serialize (TypedSymbol knd a)
  where
  put = serialize
  get = deserialize

instance
  (IsSymbolKind knd, Typeable a) =>
  Binary.Binary (TypedSymbol knd a)
  where
  put = serialize
  get = deserialize

conTermTag :: Word8
conTermTag = 0

symTermTag :: Word8
symTermTag = 1

forallTermTag :: Word8
forallTermTag = 2

existsTermTag :: Word8
existsTermTag = 3

notTermTag :: Word8
notTermTag = 4

orTermTag :: Word8
orTermTag = 5

andTermTag :: Word8
andTermTag = 6

eqTermTag :: Word8
eqTermTag = 7

distinctTermTag :: Word8
distinctTermTag = 8

iteTermTag :: Word8
iteTermTag = 9

addNumTermTag :: Word8
addNumTermTag = 10

negNumTermTag :: Word8
negNumTermTag = 11

mulNumTermTag :: Word8
mulNumTermTag = 12

absNumTermTag :: Word8
absNumTermTag = 13

signumNumTermTag :: Word8
signumNumTermTag = 14

ltOrdTermTag :: Word8
ltOrdTermTag = 15

leOrdTermTag :: Word8
leOrdTermTag = 16

andBitsTermTag :: Word8
andBitsTermTag = 17

orBitsTermTag :: Word8
orBitsTermTag = 18

xorBitsTermTag :: Word8
xorBitsTermTag = 19

complementBitsTermTag :: Word8
complementBitsTermTag = 20

shiftLeftTermTag :: Word8
shiftLeftTermTag = 21

shiftRightTermTag :: Word8
shiftRightTermTag = 22

rotateLeftTermTag :: Word8
rotateLeftTermTag = 23

rotateRightTermTag :: Word8
rotateRightTermTag = 24

terminalTag :: Word8
terminalTag = 255

asBoolTerm :: (HasCallStack) => SomeTerm -> Term Bool
asBoolTerm (SomeTerm (t :: Term a)) =
  case eqTypeRep (primTypeRep @Bool) (primTypeRep @a) of
    Just HRefl -> t
    Nothing -> error "asBoolTerm: type mismatch"

asSameTypeNonEmptyTermList ::
  (HasCallStack) =>
  NonEmpty SomeTerm ->
  (forall b. NonEmpty (Term b) -> r) ->
  r
asSameTypeNonEmptyTermList (SomeTerm (t :: Term a) :| ts) f =
  f $ t :| fmap (unsafeCastTerm t) ts
  where
    unsafeCastTerm :: Term a -> SomeTerm -> Term a
    unsafeCastTerm t (SomeTerm b) =
      introSupportedPrimConstraint t $
        case castTerm b of
          Just r -> r
          Nothing -> error "asSameTypeNonEmptyTermList: type mismatch"

asSameTypeTermPair ::
  (HasCallStack) =>
  SomeTerm ->
  SomeTerm ->
  (forall b. Term b -> Term b -> r) ->
  r
asSameTypeTermPair (SomeTerm (t1 :: Term a)) (SomeTerm (t2 :: Term b)) f =
  case eqTypeRep (primTypeRep @a) (primTypeRep @b) of
    Just HRefl -> f t1 t2
    Nothing -> error "asSameTypeTermPair: type mismatch"

asNumTypeTerm ::
  (HasCallStack) =>
  SomeTerm ->
  (forall n. (PEvalNumTerm n) => Term n -> r) ->
  r
asNumTypeTerm (SomeTerm (t1 :: Term a)) f =
  case ( eqTypeRep ta (typeRep @Integer),
         eqTypeRep ta (typeRep @AlgReal)
       ) of
    (Just HRefl, _) -> f t1
    (_, Just HRefl) -> f t1
    _ ->
      case ta of
        App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
          case ( eqTypeRep ta (typeRep @WordN),
                 eqTypeRep ta (typeRep @IntN)
               ) of
            (Just HRefl, _) -> withPrim @a $ f t1
            (_, Just HRefl) -> withPrim @a $ f t1
            _ -> err
        App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
          case eqTypeRep tf (typeRep @FP) of
            Just HRefl ->
              withPrim @a $ withPrim @a $ f t1
            _ -> err
        _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asNumTypeTerm: unsupported type: " <> show ta

asOrdTypeTerm ::
  (HasCallStack) => SomeTerm -> (forall n. (PEvalOrdTerm n) => Term n -> r) -> r
asOrdTypeTerm (SomeTerm (t1 :: Term a)) f =
  case ( eqTypeRep ta (typeRep @Integer),
         eqTypeRep ta (typeRep @AlgReal),
         eqTypeRep ta (typeRep @FPRoundingMode)
       ) of
    (Just HRefl, _, _) -> f t1
    (_, Just HRefl, _) -> f t1
    (_, _, Just HRefl) -> f t1
    _ ->
      case ta of
        App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
          case ( eqTypeRep ta (typeRep @WordN),
                 eqTypeRep ta (typeRep @IntN)
               ) of
            (Just HRefl, _) -> withPrim @a $ f t1
            (_, Just HRefl) -> withPrim @a $ f t1
            _ -> err
        App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
          case eqTypeRep tf (typeRep @FP) of
            Just HRefl ->
              withPrim @a $ withPrim @a $ f t1
            _ -> err
        _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asOrdTypeTerm: unsupported type: " <> show ta

asBitsTypeTerm ::
  (HasCallStack) =>
  SomeTerm ->
  ( forall n.
    (PEvalBitwiseTerm n, PEvalShiftTerm n, PEvalRotateTerm n) =>
    Term n ->
    r
  ) ->
  r
asBitsTypeTerm (SomeTerm (t1 :: Term a)) f =
  case ta of
    App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
      case ( eqTypeRep ta (typeRep @WordN),
             eqTypeRep ta (typeRep @IntN)
           ) of
        (Just HRefl, _) -> withPrim @a $ f t1
        (_, Just HRefl) -> withPrim @a $ f t1
        _ -> err
    _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asOrdTypeTerm: unsupported type: " <> show ta

asNumTypeTermPair ::
  (HasCallStack) =>
  SomeTerm ->
  SomeTerm ->
  (forall n. (PEvalNumTerm n) => Term n -> Term n -> r) ->
  r
asNumTypeTermPair (SomeTerm (t1 :: Term a)) (SomeTerm (t2 :: Term b)) f =
  asNumTypeTerm (SomeTerm t1) $ \(t1' :: Term a1) ->
    introSupportedPrimConstraint t1' $
      case (eqTypeRep (primTypeRep @a1) tb) of
        Just HRefl ->
          f t1' t2
        _ -> err
  where
    ta = primTypeRep @a
    tb = primTypeRep @b
    err = error $ "asNumTypeTermPair: unsupported type: " <> show ta <> show tb

asOrdTypeTermPair ::
  (HasCallStack) =>
  SomeTerm ->
  SomeTerm ->
  (forall n. (PEvalOrdTerm n) => Term n -> Term n -> r) ->
  r
asOrdTypeTermPair (SomeTerm (t1 :: Term a)) (SomeTerm (t2 :: Term b)) f =
  asOrdTypeTerm (SomeTerm t1) $ \(t1' :: Term a1) ->
    introSupportedPrimConstraint t1' $
      case (eqTypeRep (primTypeRep @a1) tb) of
        Just HRefl ->
          f t1' t2
        _ -> err
  where
    ta = primTypeRep @a
    tb = primTypeRep @b
    err = error $ "asOrdTypeTermPair: unsupported type: " <> show ta <> show tb

asBitsTypeTermPair ::
  (HasCallStack) =>
  SomeTerm ->
  SomeTerm ->
  ( forall n.
    (PEvalBitwiseTerm n, PEvalShiftTerm n, PEvalRotateTerm n) =>
    Term n ->
    Term n ->
    r
  ) ->
  r
asBitsTypeTermPair (SomeTerm (t1 :: Term a)) (SomeTerm (t2 :: Term b)) f =
  asBitsTypeTerm (SomeTerm t1) $ \(t1' :: Term a1) ->
    introSupportedPrimConstraint t1' $
      case (eqTypeRep (primTypeRep @a1) tb) of
        Just HRefl ->
          f t1' t2
        _ -> err
  where
    ta = primTypeRep @a
    tb = primTypeRep @b
    err = error $ "asOrdTypeTermPair: unsupported type: " <> show ta <> show tb

statefulDeserializeSomeTerm ::
  (MonadGet m) => StateT (HM.HashMap Id SomeTerm, SomeTerm) m SomeTerm
statefulDeserializeSomeTerm = do
  r <- do
    tmId <- deserialize
    tag <- getWord8
    if
      | tag == conTermTag -> do
          knownType <- deserializeKnownType
          case witnessKnownType knownType of
            KnownTypeWitness (Proxy :: Proxy a) -> do
              tm <- someTerm . conTerm <$> deserialize @a
              return $ Just (tm, tmId)
      | tag == symTermTag -> do
          SomeTypedSymbol sb <- deserialize @SomeTypedAnySymbol
          return $ Just (someTerm $ symTerm sb, tmId)
      | tag == forallTermTag -> deserializeQuantified tmId forallTerm
      | tag == existsTermTag -> deserializeQuantified tmId existsTerm
      | tag == notTermTag -> do
          t <- deserializeTerm
          return $ Just (someTerm $ notTerm $ asBoolTerm t, tmId)
      | tag == orTermTag -> deserializeBoolBinary tmId orTerm
      | tag == andTermTag -> deserializeBoolBinary tmId andTerm
      | tag == eqTermTag -> do
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          asSameTypeTermPair t1 t2 $ \t1' t2' ->
            return $ Just (someTerm $ eqTerm t1' t2', tmId)
      | tag == distinctTermTag -> do
          ts <- deserializeNonEmptyTermList
          asSameTypeNonEmptyTermList ts $ \ts' ->
            return $ Just (someTerm $ distinctTerm ts', tmId)
      | tag == iteTermTag -> do
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          t3 <- deserializeTerm
          asSameTypeTermPair t2 t3 $ \t2' t3' ->
            return $ Just (someTerm $ iteTerm (asBoolTerm t1) t2' t3', tmId)
      | tag == addNumTermTag -> deserializeNumBinary tmId addNumTerm
      | tag == negNumTermTag -> deserializeNumUnary tmId negNumTerm
      | tag == mulNumTermTag -> deserializeNumBinary tmId mulNumTerm
      | tag == absNumTermTag -> deserializeNumUnary tmId absNumTerm
      | tag == signumNumTermTag -> deserializeNumUnary tmId signumNumTerm
      | tag == ltOrdTermTag -> deserializeOrdBinary tmId ltOrdTerm
      | tag == leOrdTermTag -> deserializeOrdBinary tmId leOrdTerm
      | tag == andBitsTermTag -> deserializeBitsBinary tmId andBitsTerm
      | tag == orBitsTermTag -> deserializeBitsBinary tmId orBitsTerm
      | tag == xorBitsTermTag -> deserializeBitsBinary tmId xorBitsTerm
      | tag == complementBitsTermTag ->
          deserializeBitsUnary tmId complementBitsTerm
      | tag == shiftLeftTermTag -> deserializeBitsBinary tmId shiftLeftTerm
      | tag == shiftRightTermTag -> deserializeBitsBinary tmId shiftRightTerm
      | tag == rotateLeftTermTag -> deserializeBitsBinary tmId rotateLeftTerm
      | tag == rotateRightTermTag -> deserializeBitsBinary tmId rotateRightTerm
      | tag == terminalTag -> return Nothing
      | otherwise ->
          error $ "statefulDeserializeSomeTerm: unknown tag: " <> show tag
  case r of
    Just (tm, tmId) -> do
      State.modify' $ \(m, _) -> (HM.insert tmId tm m, tm)
      statefulDeserializeSomeTerm
    Nothing -> State.gets snd
  where
    deserializeNonEmptyTermList ::
      (MonadGet m) =>
      StateT (HM.HashMap Id SomeTerm, SomeTerm) m (NonEmpty SomeTerm)
    deserializeNonEmptyTermList = do
      ids <- deserialize @[Id]
      case ids of
        [] -> fail "statefulDeserializeSomeTerm: empty list"
        (x : xs) -> do
          x' <- queryTerm x
          xs' <- traverse queryTerm xs
          return $ x' :| xs'
    deserializeTerm ::
      (MonadGet m) => StateT (HM.HashMap Id SomeTerm, SomeTerm) m SomeTerm
    deserializeTerm = do
      tmId <- deserialize
      queryTerm tmId
    queryTerm ::
      (MonadGet m) => Id -> StateT (HM.HashMap Id SomeTerm, SomeTerm) m SomeTerm
    queryTerm termId = do
      tm <- State.gets $ HM.lookup termId . fst
      case tm of
        Nothing -> fail "statefulDeserializeSomeTerm: unknown term id"
        Just tm' -> return tm'
    deserializeBoolBinary tmId f = do
      t1 <- deserializeTerm
      t2 <- deserializeTerm
      return $
        Just (someTerm $ f (asBoolTerm t1) (asBoolTerm t2), tmId)
    deserializeQuantified
      tmId
      (f :: forall t. TypedConstantSymbol t -> Term Bool -> Term Bool) = do
        SomeTypedSymbol sb <- deserialize
        t <- deserializeTerm
        return $ Just (someTerm $ f sb $ asBoolTerm t, tmId)
    deserializeNumUnary
      tmId
      (f :: forall t. (PEvalNumTerm t) => Term t -> Term t) = do
        t1 <- deserializeTerm
        asNumTypeTerm t1 $ \t1' -> return $ Just (someTerm $ f t1', tmId)
    deserializeBitsUnary
      tmId
      (f :: forall t. (PEvalBitwiseTerm t) => Term t -> Term t) = do
        t1 <- deserializeTerm
        asBitsTypeTerm t1 $ \t1' -> return $ Just (someTerm $ f t1', tmId)
    deserializeNumBinary
      tmId
      (f :: forall t. (PEvalNumTerm t) => Term t -> Term t -> Term t) = do
        t1 <- deserializeTerm
        t2 <- deserializeTerm
        asNumTypeTermPair t1 t2 $ \t1' t2' ->
          return $
            Just (someTerm $ f t1' t2', tmId)
    deserializeOrdBinary
      tmId
      (f :: forall t. (PEvalOrdTerm t) => Term t -> Term t -> Term Bool) = do
        t1 <- deserializeTerm
        t2 <- deserializeTerm
        asOrdTypeTermPair t1 t2 $ \t1' t2' ->
          return $
            Just (someTerm $ f t1' t2', tmId)
    deserializeBitsBinary
      tmId
      ( f ::
          forall t.
          ( PEvalBitwiseTerm t,
            PEvalShiftTerm t,
            PEvalRotateTerm t
          ) =>
          Term t ->
          Term t ->
          Term t
        ) = do
        t1 <- deserializeTerm
        t2 <- deserializeTerm
        asBitsTypeTermPair t1 t2 $ \t1' t2' ->
          return $
            Just (someTerm $ f t1' t2', tmId)

deserializeSomeTerm :: (MonadGet m) => m SomeTerm
deserializeSomeTerm =
  evalStateT
    statefulDeserializeSomeTerm
    ( HM.empty,
      error $
        "deserializeSomeTerm: should not happen: started with the terminal "
          <> "value"
    )

serializeSingleSomeTerm ::
  (MonadPut m) => SomeTerm -> StateT (HS.HashSet Id) m ()
serializeSingleSomeTerm (SomeTerm tm) = do
  st <- State.get
  let tmId = termId tm
  if HS.member tmId st
    then return ()
    else do
      case tm of
        ConTerm _ _ _ _ (v :: v) -> do
          serialize tmId
          putWord8 conTermTag
          let kt = knownType (Proxy @v)
          case witnessKnownType kt of
            KnownTypeWitness (Proxy :: Proxy v1) ->
              case eqTypeRep (primTypeRep @v) (typeRep @v1) of
                Just HRefl -> do
                  serializeKnownType kt
                  serialize v
                Nothing ->
                  error "serializeSomeTerm: should not happen: type mismatch"
        SymTerm _ _ _ _ (v :: TypedAnySymbol v) -> do
          serialize tmId
          putWord8 symTermTag
          serialize $ someTypedSymbol v
        ForallTerm _ _ _ _ ts t -> serializeQuantified tmId forallTermTag ts t
        ExistsTerm _ _ _ _ ts t -> serializeQuantified tmId existsTermTag ts t
        NotTerm _ _ _ _ t -> do
          serializeSingleSomeTerm $ someTerm t
          do
            serialize tmId
            putWord8 notTermTag
            serialize $ termId t
        OrTerm _ _ _ _ t1 t2 -> serializeBinary tmId orTermTag t1 t2
        AndTerm _ _ _ _ t1 t2 -> serializeBinary tmId andTermTag t1 t2
        EqTerm _ _ _ _ t1 t2 -> serializeBinary tmId eqTermTag t1 t2
        ITETerm _ _ _ _ t1 t2 t3 -> serializeTernary tmId iteTermTag t1 t2 t3
        AddNumTerm _ _ _ _ t1 t2 -> serializeBinary tmId addNumTermTag t1 t2
        NegNumTerm _ _ _ _ t -> serializeUnary tmId negNumTermTag t
        MulNumTerm _ _ _ _ t1 t2 -> serializeBinary tmId mulNumTermTag t1 t2
        AbsNumTerm _ _ _ _ t -> serializeUnary tmId absNumTermTag t
        SignumNumTerm _ _ _ _ t -> serializeUnary tmId signumNumTermTag t
        LtOrdTerm _ _ _ _ t1 t2 -> serializeBinary tmId ltOrdTermTag t1 t2
        LeOrdTerm _ _ _ _ t1 t2 -> serializeBinary tmId leOrdTermTag t1 t2
        AndBitsTerm _ _ _ _ t1 t2 -> serializeBinary tmId andBitsTermTag t1 t2
        OrBitsTerm _ _ _ _ t1 t2 -> serializeBinary tmId orBitsTermTag t1 t2
        XorBitsTerm _ _ _ _ t1 t2 -> serializeBinary tmId xorBitsTermTag t1 t2
        ComplementBitsTerm _ _ _ _ t ->
          serializeUnary tmId complementBitsTermTag t
        ShiftLeftTerm _ _ _ _ t1 t2 ->
          serializeBinary tmId shiftLeftTermTag t1 t2
        ShiftRightTerm _ _ _ _ t1 t2 ->
          serializeBinary tmId shiftRightTermTag t1 t2
        RotateLeftTerm _ _ _ _ t1 t2 ->
          serializeBinary tmId rotateLeftTermTag t1 t2
        RotateRightTerm _ _ _ _ t1 t2 ->
          serializeBinary tmId rotateRightTermTag t1 t2
        _ -> error "serializeSomeTerm: unsupported term"
  State.put $ HS.insert (termId tm) st
  where
    serializeQuantified ::
      (MonadPut m) =>
      Id ->
      Word8 ->
      TypedConstantSymbol v ->
      Term b ->
      StateT (HS.HashSet Id) m ()
    serializeQuantified tmId tag v t = do
      serializeSingleSomeTerm $ someTerm t
      serialize tmId
      serialize tag
      serialize $ someTypedSymbol v
      serialize $ termId t
    serializeUnary tmId tag t1 = do
      serializeSingleSomeTerm $ someTerm t1
      serialize tmId
      serialize tag
      serialize $ termId t1
    serializeBinary tmId tag t1 t2 = do
      serializeSingleSomeTerm $ someTerm t1
      serializeSingleSomeTerm $ someTerm t2
      serialize tmId
      serialize tag
      serialize $ termId t1
      serialize $ termId t2
    serializeTernary tmId tag t1 t2 t3 = do
      serializeSingleSomeTerm $ someTerm t1
      serializeSingleSomeTerm $ someTerm t2
      serializeSingleSomeTerm $ someTerm t3
      serialize tmId
      serialize tag
      serialize $ termId t1
      serialize $ termId t2
      serialize $ termId t3

serializeSomeTerm :: (MonadPut m) => SomeTerm -> m ()
serializeSomeTerm t = do
  flip evalStateT HS.empty $ serializeSingleSomeTerm t
  serialize (0 :: Id)
  putWord8 terminalTag

instance Serial SomeTerm where
  serialize = serializeSomeTerm
  deserialize = deserializeSomeTerm

instance Cereal.Serialize SomeTerm where
  put = serializeSomeTerm
  get = deserializeSomeTerm

instance Binary.Binary SomeTerm where
  put = serializeSomeTerm
  get = deserializeSomeTerm

instance (SupportedPrim a) => Serial (Term a) where
  serialize = serializeSomeTerm . someTerm
  deserialize = do
    SomeTerm tm <- deserialize
    withSupportedPrimTypeable @a $ case castTerm tm of
      Just r -> return r
      Nothing -> fail "deserialize Term: type mismatch"

instance (SupportedPrim a) => Cereal.Serialize (Term a) where
  put = serialize
  get = deserialize

instance (SupportedPrim a) => Binary.Binary (Term a) where
  put = serialize
  get = deserialize

instance (GeneralFunArg a, GeneralFunArg b) => Serial (a --> b) where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance (GeneralFunArg a, GeneralFunArg b) => Cereal.Serialize (a --> b) where
  put = serialize
  get = deserialize

instance (GeneralFunArg a, GeneralFunArg b) => Binary.Binary (a --> b) where
  put = serialize
  get = deserialize

type GeneralFunArg t = (SupportedNonFuncPrim t, Typeable t, Show t, Hashable t)

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c) =>
  Serial (a --> b --> c)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c) =>
  Cereal.Serialize (a --> b --> c)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c) =>
  Binary.Binary (a --> b --> c)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c, GeneralFunArg d) =>
  Serial (a --> b --> c --> d)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c, GeneralFunArg d) =>
  Cereal.Serialize (a --> b --> c --> d)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c, GeneralFunArg d) =>
  Binary.Binary (a --> b --> c --> d)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e
  ) =>
  Serial (a --> b --> c --> d --> e)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e
  ) =>
  Binary.Binary (a --> b --> c --> d --> e)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f
  ) =>
  Serial (a --> b --> c --> d --> e --> f)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g
  ) =>
  Serial (a --> b --> c --> d --> e --> f --> g)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f --> g)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f --> g)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h
  ) =>
  Serial (a --> b --> c --> d --> e --> f --> g --> h)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f --> g --> h)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f --> g --> h)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h,
    GeneralFunArg i
  ) =>
  Serial (a --> b --> c --> d --> e --> f --> g --> h --> i)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h,
    GeneralFunArg i
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f --> g --> h --> i)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h,
    GeneralFunArg i
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f --> g --> h --> i)
  where
  put = serialize
  get = deserialize
