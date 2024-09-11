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
import Control.Monad.State (MonadTrans (lift), StateT, evalStateT)
import qualified Control.Monad.State as State
import Data.Foldable (traverse_)
import qualified Data.HashMap.Internal.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import Data.Serialize
  ( Get,
    PutM,
    Putter,
    Serialize (get, put),
    getWord8,
    putWord8,
  )
import Data.Vector.Internal.Check (HasCallStack)
import Data.Word (Word8)
import GHC.TypeNats (KnownNat, Natural, natVal, type (<=))
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
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind (decideSymbolKind),
    PEvalNumTerm,
    SomeTypedAnySymbol,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedNonFuncPrim,
    SupportedPrim (primTypeRep, withPrim),
    Term
      ( AbsNumTerm,
        AddNumTerm,
        AndTerm,
        ConTerm,
        EqTerm,
        ExistsTerm,
        ForallTerm,
        ITETerm,
        MulNumTerm,
        NegNumTerm,
        NotTerm,
        OrTerm,
        SignumNumTerm,
        SymTerm
      ),
    TypedAnySymbol,
    TypedConstantSymbol,
    TypedSymbol (TypedSymbol),
    absNumTerm,
    addNumTerm,
    andTerm,
    conTerm,
    distinctTerm,
    eqTerm,
    existsTerm,
    forallTerm,
    introSupportedPrimConstraint,
    iteTerm,
    mulNumTerm,
    negNumTerm,
    notTerm,
    orTerm,
    signumNumTerm,
    someTypedSymbol,
    symTerm,
    termId,
    withSupportedPrimTypeable,
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
      Serialize a
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
      Serialize a
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
putKnownNonFuncType :: Putter KnownNonFuncType
putKnownNonFuncType BoolType = putWord8 0
putKnownNonFuncType IntegerType = putWord8 1
putKnownNonFuncType (WordNType (Proxy :: Proxy n)) =
  putWord8 2 >> put (natVal (Proxy @n))
putKnownNonFuncType (IntNType (Proxy :: Proxy n)) =
  putWord8 3 >> put (natVal (Proxy @n))
putKnownNonFuncType (FPType (Proxy :: Proxy eb) (Proxy :: Proxy sb)) =
  putWord8 4 >> put (natVal (Proxy @eb)) >> put (natVal (Proxy @sb))
putKnownNonFuncType FPRoundingModeType = putWord8 5
putKnownNonFuncType AlgRealType = putWord8 6

putKnownType :: Putter KnownType
putKnownType (NonFuncType t) = putWord8 0 >> putKnownNonFuncType t
putKnownType (TabularFunType ts) =
  putWord8 1
    >> putWord8 (fromIntegral $ length ts)
    >> traverse_ putKnownNonFuncType ts
putKnownType (GeneralFunType ts) =
  putWord8 2
    >> putWord8 (fromIntegral $ length ts)
    >> traverse_ putKnownNonFuncType ts

getKnownNonFuncType :: Get KnownNonFuncType
getKnownNonFuncType = do
  tag <- getWord8
  case tag of
    0 -> return BoolType
    1 -> return IntegerType
    2 -> do
      n <- get @Natural
      when (n == 0) $ fail "WordN 0 is not allowed"
      case mkPositiveNatRepr n of
        SomePositiveNatRepr (_ :: NatRepr n) -> return $ WordNType (Proxy @n)
    3 -> do
      n <- get @Natural
      when (n == 0) $ fail "IntN 0 is not allowed"
      case mkPositiveNatRepr n of
        SomePositiveNatRepr (_ :: NatRepr n) -> return $ IntNType (Proxy @n)
    4 -> do
      eb <- get @Natural
      sb <- get @Natural
      unless (checkDynamicValidFP eb sb) $ fail invalidFPMessage
      case (mkPositiveNatRepr eb, mkPositiveNatRepr sb) of
        ( SomePositiveNatRepr (_ :: NatRepr eb),
          SomePositiveNatRepr (_ :: NatRepr sb)
          ) ->
            withUnsafeValidFP @eb @sb $ return $ FPType (Proxy @eb) (Proxy @sb)
    5 -> return FPRoundingModeType
    6 -> return AlgRealType
    _ -> fail "Unknown type tag"

getKnownType :: Get KnownType
getKnownType = do
  tag <- getWord8
  case tag of
    0 -> NonFuncType <$> getKnownNonFuncType
    1 -> do
      n <- getWord8
      nfs <- replicateM (fromIntegral n) getKnownNonFuncType
      return $ TabularFunType nfs
    2 -> do
      n <- getWord8
      nfs <- replicateM (fromIntegral n) getKnownNonFuncType
      return $ GeneralFunType nfs
    _ -> fail "Unknown type tag"

instance (IsSymbolKind knd) => Serialize (SomeTypedSymbol knd) where
  put (SomeTypedSymbol tsb@(TypedSymbol sb)) =
    case decideSymbolKind @knd of
      Left HRefl -> do
        putKnownNonFuncType $ knownNonFuncType tsb
        put sb
      Right HRefl -> do
        putKnownType $ knownType tsb
        put sb
  get = case decideSymbolKind @knd of
    Left HRefl -> do
      kt <- getKnownNonFuncType
      case witnessKnownNonFuncType kt of
        KnownNonFuncTypeWitness (Proxy :: Proxy a) -> do
          sb <- get
          return $ SomeTypedSymbol $ TypedSymbol @a sb
    Right HRefl -> do
      kt <- getKnownType
      case witnessKnownType kt of
        KnownTypeWitness (Proxy :: Proxy a) -> do
          sb <- get
          return $ SomeTypedSymbol $ TypedSymbol @a sb

instance (IsSymbolKind knd, Typeable a) => Serialize (TypedSymbol knd a) where
  put tsb = put $ someTypedSymbol tsb
  get = do
    SomeTypedSymbol (tsb@TypedSymbol {} :: TypedSymbol knd b) <- get
    case eqTypeRep (typeRep @a) (primTypeRep @b) of
      Just HRefl -> return tsb
      Nothing -> fail "get TypedSymbol: type mismatch"

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
    err = error $ "asNumTypeTermPair: unsupported type: " <> show ta

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

statefulGetSomeTerm ::
  StateT (HM.HashMap Id SomeTerm, SomeTerm) Get SomeTerm
statefulGetSomeTerm = do
  r <- do
    tmId <- lift get
    tag <- lift getWord8
    if
      | tag == conTermTag -> lift $ do
          knownType <- getKnownType
          case witnessKnownType knownType of
            KnownTypeWitness (Proxy :: Proxy a) -> do
              tm <- someTerm . conTerm <$> get @a
              return $ Just (tm, tmId)
      | tag == symTermTag -> lift $ do
          SomeTypedSymbol sb <- get @SomeTypedAnySymbol
          return $ Just (someTerm $ symTerm sb, tmId)
      | tag == forallTermTag -> getQuantified tmId forallTerm
      | tag == existsTermTag -> getQuantified tmId existsTerm
      | tag == notTermTag -> do
          t <- getTerm
          return $ Just (someTerm $ notTerm $ asBoolTerm t, tmId)
      | tag == orTermTag -> getBoolBinary tmId orTerm
      | tag == andTermTag -> getBoolBinary tmId andTerm
      | tag == eqTermTag -> do
          t1 <- getTerm
          t2 <- getTerm
          asSameTypeTermPair t1 t2 $ \t1' t2' ->
            return $ Just (someTerm $ eqTerm t1' t2', tmId)
      | tag == distinctTermTag -> do
          ts <- getNonEmptyTermList
          asSameTypeNonEmptyTermList ts $ \ts' ->
            return $ Just (someTerm $ distinctTerm ts', tmId)
      | tag == iteTermTag -> do
          t1 <- getTerm
          t2 <- getTerm
          t3 <- getTerm
          asSameTypeTermPair t2 t3 $ \t2' t3' ->
            return $ Just (someTerm $ iteTerm (asBoolTerm t1) t2' t3', tmId)
      | tag == addNumTermTag -> getNumBinary tmId addNumTerm
      | tag == negNumTermTag -> getNumUnary tmId negNumTerm
      | tag == mulNumTermTag -> getNumBinary tmId mulNumTerm
      | tag == absNumTermTag -> getNumUnary tmId absNumTerm
      | tag == signumNumTermTag -> getNumUnary tmId signumNumTerm
      | tag == terminalTag -> return Nothing
      | otherwise -> error $ "statefulGetSomeTerm: unknown tag: " <> show tag
  case r of
    Just (tm, tmId) -> do
      State.modify' $ \(m, _) -> (HM.insert tmId tm m, tm)
      statefulGetSomeTerm
    Nothing -> State.gets snd
  where
    getNonEmptyTermList ::
      StateT (HM.HashMap Id SomeTerm, SomeTerm) Get (NonEmpty SomeTerm)
    getNonEmptyTermList = do
      ids <- lift $ get @[Id]
      case ids of
        [] -> fail "statefulGetSomeTerm: empty list"
        (x : xs) -> do
          x' <- queryTerm x
          xs' <- traverse queryTerm xs
          return $ x' :| xs'
    getTerm :: StateT (HM.HashMap Id SomeTerm, SomeTerm) Get SomeTerm
    getTerm = do
      tmId <- lift get
      queryTerm tmId
    queryTerm :: Id -> StateT (HM.HashMap Id SomeTerm, SomeTerm) Get SomeTerm
    queryTerm termId = do
      tm <- State.gets $ HM.lookup termId . fst
      case tm of
        Nothing -> fail "statefulGetSomeTerm: unknown term id"
        Just tm' -> return tm'
    getBoolBinary tmId f = do
      t1 <- getTerm
      t2 <- getTerm
      return $
        Just (someTerm $ f (asBoolTerm t1) (asBoolTerm t2), tmId)
    getQuantified
      tmId
      (f :: forall t. TypedConstantSymbol t -> Term Bool -> Term Bool) = do
        SomeTypedSymbol sb <- lift get
        t <- getTerm
        return $ Just (someTerm $ f sb $ asBoolTerm t, tmId)
    getNumUnary
      tmId
      (f :: forall t. (PEvalNumTerm t) => Term t -> Term t) = do
        t1 <- getTerm
        asNumTypeTerm t1 $ \t1' ->
          return $
            Just (someTerm $ f t1', tmId)
    getNumBinary
      tmId
      (f :: forall t. (PEvalNumTerm t) => Term t -> Term t -> Term t) = do
        t1 <- getTerm
        t2 <- getTerm
        asNumTypeTermPair t1 t2 $ \t1' t2' ->
          return $
            Just (someTerm $ f t1' t2', tmId)

getSomeTerm :: Get SomeTerm
getSomeTerm =
  evalStateT
    statefulGetSomeTerm
    ( HM.empty,
      error "getSomeTerm: should not happen: started with the terminal value"
    )

putSingleSomeTerm :: SomeTerm -> StateT (HS.HashSet Id) PutM ()
putSingleSomeTerm (SomeTerm tm) = do
  st <- State.get
  let tmId = termId tm
  if HS.member tmId st
    then return ()
    else do
      case tm of
        ConTerm _ _ _ _ (v :: v) -> lift $ do
          put tmId
          putWord8 conTermTag
          let kt = knownType (Proxy @v)
          case witnessKnownType kt of
            KnownTypeWitness (Proxy :: Proxy v1) ->
              case eqTypeRep (primTypeRep @v) (typeRep @v1) of
                Just HRefl -> do
                  putKnownType kt
                  put v
                Nothing -> error "putSomeTerm: should not happen: type mismatch"
        SymTerm _ _ _ _ (v :: TypedAnySymbol v) -> lift $ do
          put tmId
          putWord8 symTermTag
          put $ someTypedSymbol v
        ForallTerm _ _ _ _ ts t -> putQuantified tmId forallTermTag ts t
        ExistsTerm _ _ _ _ ts t -> putQuantified tmId existsTermTag ts t
        NotTerm _ _ _ _ t -> do
          putSingleSomeTerm $ someTerm t
          lift $ do
            put tmId
            putWord8 notTermTag
            put $ termId t
        OrTerm _ _ _ _ t1 t2 -> putBinary tmId orTermTag t1 t2
        AndTerm _ _ _ _ t1 t2 -> putBinary tmId andTermTag t1 t2
        EqTerm _ _ _ _ t1 t2 -> putBinary tmId eqTermTag t1 t2
        ITETerm _ _ _ _ t1 t2 t3 -> putTernary tmId iteTermTag t1 t2 t3
        AddNumTerm _ _ _ _ t1 t2 -> putBinary tmId addNumTermTag t1 t2
        NegNumTerm _ _ _ _ t -> putUnary tmId negNumTermTag t
        MulNumTerm _ _ _ _ t1 t2 -> putBinary tmId mulNumTermTag t1 t2
        AbsNumTerm _ _ _ _ t -> putUnary tmId absNumTermTag t
        SignumNumTerm _ _ _ _ t -> putUnary tmId signumNumTermTag t
        _ -> error "putSomeTerm: unsupported term"
  State.put $ HS.insert (termId tm) st
  where
    putQuantified ::
      Id ->
      Word8 ->
      TypedConstantSymbol v ->
      Term b ->
      StateT (HS.HashSet Id) PutM ()
    putQuantified tmId tag v t = do
      putSingleSomeTerm $ someTerm t
      lift $ do
        put tmId
        put tag
        put $ someTypedSymbol v
        put $ termId t
    putUnary tmId tag t1 = do
      putSingleSomeTerm $ someTerm t1
      lift $ do
        put tmId
        put tag
        put $ termId t1
    putBinary tmId tag t1 t2 = do
      putSingleSomeTerm $ someTerm t1
      putSingleSomeTerm $ someTerm t2
      lift $ do
        put tmId
        put tag
        put $ termId t1
        put $ termId t2
    putTernary tmId tag t1 t2 t3 = do
      putSingleSomeTerm $ someTerm t1
      putSingleSomeTerm $ someTerm t2
      putSingleSomeTerm $ someTerm t3
      lift $ do
        put tmId
        put tag
        put $ termId t1
        put $ termId t2
        put $ termId t3

putSomeTerm :: Putter SomeTerm
putSomeTerm t = do
  flip evalStateT HS.empty $ putSingleSomeTerm t
  put (0 :: Id)
  putWord8 terminalTag

instance Serialize SomeTerm where
  put = putSomeTerm
  get = getSomeTerm

instance (SupportedPrim a) => Serialize (Term a) where
  put = putSomeTerm . someTerm
  get = do
    SomeTerm tm <- get
    withSupportedPrimTypeable @a $ case castTerm tm of
      Just r -> return r
      Nothing -> fail "get Term: type mismatch"

instance
  (GeneralFunArg a, GeneralFunArg b) =>
  Serialize (a --> b)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get

type GeneralFunArg t = (SupportedNonFuncPrim t, Typeable t, Show t, Hashable t)

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c) =>
  Serialize (a --> b --> c)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c, GeneralFunArg d) =>
  Serialize (a --> b --> c --> d)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e
  ) =>
  Serialize (a --> b --> c --> d --> e)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f
  ) =>
  Serialize (a --> b --> c --> d --> e --> f)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get

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
  Serialize (a --> b --> c --> d --> e --> f --> g)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get

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
  Serialize (a --> b --> c --> d --> e --> f --> g --> h)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get

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
  Serialize (a --> b --> c --> d --> e --> f --> g --> h --> i)
  where
  put (GeneralFun ts tm) = put ts >> put tm
  get = GeneralFun <$> get <*> get
