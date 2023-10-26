{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.GenSym
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.GenSym
  ( -- * Indices and identifiers for fresh symbolic value generation
    FreshIndex (..),
    FreshIdent (..),
    name,
    nameWithInfo,

    -- * Monad for fresh symbolic value generation
    MonadFresh (..),
    FreshT (FreshT, runFreshTFromIndex),
    Fresh,
    runFreshT,
    runFresh,
    mrgRunFreshT,

    -- * Symbolic value generation
    GenSym (..),
    GenSymSimple (..),
    genSym,
    genSymSimple,
    derivedNoSpecFresh,
    derivedNoSpecSimpleFresh,
    derivedSameShapeSimpleFresh,

    -- * Symbolic choices
    chooseFresh,
    chooseSimpleFresh,
    chooseUnionFresh,
    choose,
    chooseSimple,
    chooseUnion,

    -- * Some common GenSym specifications
    ListSpec (..),
    SimpleListSpec (..),
    EnumGenBound (..),
    EnumGenUpperBound (..),
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError (catchError, throwError),
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.RWS.Class
  ( MonadRWS,
    MonadReader (ask, local),
    MonadState (get, put),
    MonadWriter (listen, pass, writer),
    asks,
    gets,
  )
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.Signatures (Catch)
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Class
  ( MonadTrans (lift),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as B
import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Typeable
  ( Proxy (Proxy),
    Typeable,
    eqT,
    typeRep,
    type (:~:) (Refl),
  )
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Generics.Deriving
  ( Generic (Rep, from, to),
    K1 (K1),
    M1 (M1),
    U1 (U1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Core.Control.Monad.UnionM (UnionM, isMerged, underlyingUnion)
import Grisette.Core.Data.BV (IntN, SomeIntN, SomeWordN, WordN)
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    MergingStrategy (SimpleStrategy),
    rootStrategy1,
    wrapStrategy,
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    UnionLike (mergeWithStrategy, mrgIfWithStrategy, single, unionIf),
    merge,
    mrgIf,
    mrgSingle,
  )
import Grisette.Core.Data.Class.Solvable
  ( Solvable (iinfosym, isym),
  )
import Grisette.Core.Data.Union (Union (UnionIf, UnionSingle))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN (SomeSymIntN),
    SomeSymWordN (SomeSymWordN),
    SymBool,
    SymIntN,
    SymInteger,
    SymWordN,
    type (-~>),
    type (=~>),
  )
import Grisette.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    unsafeKnownProof,
    unsafeLeqProof,
  )
import Language.Haskell.TH.Syntax (Lift (liftTyped))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- | Index type used for 'GenSym'.
--
-- To generate fresh variables, a monadic stateful context will be maintained.
-- The index should be increased every time a new symbolic constant is
-- generated.
newtype FreshIndex = FreshIndex Int
  deriving (Show)
  deriving (Eq, Ord, Num) via Int

instance Mergeable FreshIndex where
  rootStrategy = SimpleStrategy $ \_ t f -> max t f

instance SimpleMergeable FreshIndex where
  mrgIte _ = max

-- | Identifier type used for 'GenSym'
--
-- The constructor is hidden intentionally.
-- You can construct an identifier by:
--
--   * a raw name
--
--     The following two expressions will refer to the same identifier (the
--     solver won't distinguish them and would assign the same value to them).
--     The user may need to use unique names to avoid unintentional identifier
--     collision.
--
--     >>> name "a"
--     a
--
--     >>> "a" :: FreshIdent -- available when OverloadedStrings is enabled
--     a
--
--   * bundle the calling file location with the name to ensure global uniqueness
--
--     Identifiers created at different locations will not be the
--     same. The identifiers created at the same location will be the same.
--
--     >>> $$(nameWithLoc "a") -- a sample result could be "a:<interactive>:18:4-18"
--     a:<interactive>:...
--
--   * bundle the calling file location with some user provided information
--
--     Identifiers created with different name or different additional
--     information will not be the same.
--
--     >>> nameWithInfo "a" (1 :: Int)
--     a:1
data FreshIdent where
  FreshIdent :: T.Text -> FreshIdent
  FreshIdentWithInfo :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => T.Text -> a -> FreshIdent

instance Show FreshIdent where
  show (FreshIdent i) = T.unpack i
  show (FreshIdentWithInfo s i) = T.unpack s ++ ":" ++ show i

instance IsString FreshIdent where
  fromString = name . T.pack

instance Eq FreshIdent where
  FreshIdent l == FreshIdent r = l == r
  FreshIdentWithInfo l (linfo :: linfo) == FreshIdentWithInfo r (rinfo :: rinfo) = case eqT @linfo @rinfo of
    Just Refl -> l == r && linfo == rinfo
    _ -> False
  _ == _ = False

instance Ord FreshIdent where
  FreshIdent l <= FreshIdent r = l <= r
  FreshIdent _ <= _ = True
  _ <= FreshIdent _ = False
  FreshIdentWithInfo l (linfo :: linfo) <= FreshIdentWithInfo r (rinfo :: rinfo) =
    l < r
      || ( l == r
             && ( case eqT @linfo @rinfo of
                    Just Refl -> linfo <= rinfo
                    _ -> typeRep (Proxy @linfo) <= typeRep (Proxy @rinfo)
                )
         )

instance Hashable FreshIdent where
  hashWithSalt s (FreshIdent n) = s `hashWithSalt` n
  hashWithSalt s (FreshIdentWithInfo n i) = s `hashWithSalt` n `hashWithSalt` i

instance Lift FreshIdent where
  liftTyped (FreshIdent n) = [||FreshIdent n||]
  liftTyped (FreshIdentWithInfo n i) = [||FreshIdentWithInfo n i||]

instance NFData FreshIdent where
  rnf (FreshIdent n) = rnf n
  rnf (FreshIdentWithInfo n i) = rnf n `seq` rnf i

-- | Simple name identifier.
-- The same identifier refers to the same symbolic variable in the whole program.
--
-- The user may need to use unique names to avoid unintentional identifier
-- collision.
name :: T.Text -> FreshIdent
name = FreshIdent

-- | Identifier with extra information.
-- The same name with the same information
-- refers to the same symbolic variable in the whole program.
--
-- The user may need to use unique names or additional information to avoid
-- unintentional identifier collision.
nameWithInfo :: forall a. (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => T.Text -> a -> FreshIdent
nameWithInfo = FreshIdentWithInfo

-- | Monad class for fresh symbolic value generation.
--
-- The monad should be a reader monad for the 'FreshIdent' and a state monad for
-- the 'FreshIndex'.
class (Monad m) => MonadFresh m where
  -- | Increase the index by one and return the new index.
  nextFreshIndex :: m FreshIndex

  -- | Get the identifier.
  getFreshIdent :: m FreshIdent

-- | A symbolic generation monad transformer.
-- It is a reader monad transformer for identifiers and
-- a state monad transformer for indices.
--
-- Each time a fresh symbolic variable is generated, the index should be increased.
newtype FreshT m a = FreshT
  { runFreshTFromIndex :: FreshIdent -> FreshIndex -> m (a, FreshIndex)
  }

instance
  (Mergeable a, Mergeable1 m) =>
  Mergeable (FreshT m a)
  where
  rootStrategy =
    wrapStrategy (liftRootStrategy (liftRootStrategy rootStrategy1)) FreshT runFreshTFromIndex

instance (Mergeable1 m) => Mergeable1 (FreshT m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy (liftRootStrategy (liftRootStrategy2 m rootStrategy))))
      FreshT
      runFreshTFromIndex

instance
  (UnionLike m, Mergeable a) =>
  SimpleMergeable (FreshT m a)
  where
  mrgIte = mrgIf

instance
  (UnionLike m) =>
  SimpleMergeable1 (FreshT m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance
  (UnionLike m) =>
  UnionLike (FreshT m)
  where
  mergeWithStrategy s (FreshT f) =
    FreshT $ \ident index -> mergeWithStrategy (liftRootStrategy2 s rootStrategy) $ f ident index
  mrgIfWithStrategy s cond (FreshT t) (FreshT f) =
    FreshT $ \ident index -> mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond (t ident index) (f ident index)
  single x = FreshT $ \_ i -> single (x, i)
  unionIf cond (FreshT t) (FreshT f) =
    FreshT $ \ident index -> unionIf cond (t ident index) (f ident index)

-- | Run the symbolic generation with the given identifier and 0 as the initial index.
runFreshT :: (Monad m) => FreshT m a -> FreshIdent -> m a
runFreshT m ident = fst <$> runFreshTFromIndex m ident (FreshIndex 0)

mrgRunFreshT ::
  (Monad m, UnionLike m, Mergeable a) =>
  FreshT m a ->
  FreshIdent ->
  m a
mrgRunFreshT m ident = merge $ runFreshT m ident

instance (Functor f) => Functor (FreshT f) where
  fmap f (FreshT s) = FreshT $ \ident idx -> first f <$> s ident idx

instance (Applicative m, Monad m) => Applicative (FreshT m) where
  pure a = FreshT $ \_ idx -> pure (a, idx)
  FreshT fs <*> FreshT as = FreshT $ \ident idx -> do
    (f, idx') <- fs ident idx
    (a, idx'') <- as ident idx'
    return (f a, idx'')

instance (Monad m) => Monad (FreshT m) where
  (FreshT s) >>= f = FreshT $ \ident idx -> do
    (a, idx') <- s ident idx
    runFreshTFromIndex (f a) ident idx'

instance MonadTrans FreshT where
  lift x = FreshT $ \_ index -> (,index) <$> x

liftFreshTCache :: (Functor m) => Catch e m (a, FreshIndex) -> Catch e (FreshT m) a
liftFreshTCache catchE (FreshT m) h =
  FreshT $ \ident index -> m ident index `catchE` \e -> runFreshTFromIndex (h e) ident index

instance (MonadError e m) => MonadError e (FreshT m) where
  throwError = lift . throwError
  catchError = liftFreshTCache catchError

instance (MonadWriter w m) => MonadWriter w (FreshT m) where
  writer p = FreshT $ \_ index -> (,index) <$> writer p
  listen (FreshT r) = FreshT $ \ident index -> (\((a, b), c) -> ((a, c), b)) <$> listen (r ident index)
  pass (FreshT r) = FreshT $ \ident index -> pass $ (\((a, b), c) -> ((a, c), b)) <$> r ident index

instance (MonadState s m) => MonadState s (FreshT m) where
  get = FreshT $ \_ index -> gets (,index)
  put s = FreshT $ \_ index -> (,index) <$> put s

instance (MonadReader r m) => MonadReader r (FreshT m) where
  local t (FreshT r) = FreshT $ \ident index -> local t (r ident index)
  ask = FreshT $ \_ index -> asks (,index)

instance (MonadRWS r w s m) => MonadRWS r w s (FreshT m)

instance (MonadFresh m) => MonadFresh (ExceptT e m) where
  nextFreshIndex = ExceptT $ Right <$> nextFreshIndex
  getFreshIdent = ExceptT $ Right <$> getFreshIdent

instance (MonadFresh m, Monoid w) => MonadFresh (WriterLazy.WriterT w m) where
  nextFreshIndex = WriterLazy.WriterT $ (,mempty) <$> nextFreshIndex
  getFreshIdent = WriterLazy.WriterT $ (,mempty) <$> getFreshIdent

instance (MonadFresh m, Monoid w) => MonadFresh (WriterStrict.WriterT w m) where
  nextFreshIndex = WriterStrict.WriterT $ (,mempty) <$> nextFreshIndex
  getFreshIdent = WriterStrict.WriterT $ (,mempty) <$> getFreshIdent

instance (MonadFresh m) => MonadFresh (StateLazy.StateT s m) where
  nextFreshIndex = StateLazy.StateT $ \s -> (,s) <$> nextFreshIndex
  getFreshIdent = StateLazy.StateT $ \s -> (,s) <$> getFreshIdent

instance (MonadFresh m) => MonadFresh (StateStrict.StateT s m) where
  nextFreshIndex = StateStrict.StateT $ \s -> (,s) <$> nextFreshIndex
  getFreshIdent = StateStrict.StateT $ \s -> (,s) <$> getFreshIdent

instance (MonadFresh m) => MonadFresh (ReaderT r m) where
  nextFreshIndex = ReaderT $ const nextFreshIndex
  getFreshIdent = ReaderT $ const getFreshIdent

instance (MonadFresh m, Monoid w) => MonadFresh (RWSLazy.RWST r w s m) where
  nextFreshIndex = RWSLazy.RWST $ \_ s -> (,s,mempty) <$> nextFreshIndex
  getFreshIdent = RWSLazy.RWST $ \_ s -> (,s,mempty) <$> getFreshIdent

instance (MonadFresh m, Monoid w) => MonadFresh (RWSStrict.RWST r w s m) where
  nextFreshIndex = RWSStrict.RWST $ \_ s -> (,s,mempty) <$> nextFreshIndex
  getFreshIdent = RWSStrict.RWST $ \_ s -> (,s,mempty) <$> getFreshIdent

-- | 'FreshT' specialized with Identity.
type Fresh = FreshT Identity

-- | Run the symbolic generation with the given identifier and 0 as the initial index.
runFresh :: Fresh a -> FreshIdent -> a
runFresh m ident = runIdentity $ runFreshT m ident

instance (Monad m) => MonadFresh (FreshT m) where
  nextFreshIndex = FreshT $ \_ idx -> return (idx, idx + 1)
  getFreshIdent = FreshT $ curry return

-- | Class of types in which symbolic values can be generated with respect to some specification.
--
-- The result will be wrapped in a union-like monad.
-- This ensures that we can generate those types with complex merging rules.
--
-- The uniqueness of symbolic constants is managed with the a monadic context.
-- 'Fresh' and 'FreshT' can be useful.
class (Mergeable a) => GenSym spec a where
  -- | Generate a symbolic value given some specification. Within a single
  -- `MonadFresh` context, calls to `fresh` would generate unique symbolic
  -- constants.
  --
  -- The following example generates a symbolic boolean. No specification is
  -- needed.
  --
  -- >>> runFresh (fresh ()) "a" :: UnionM SymBool
  -- {a@0}
  --
  -- The following example generates booleans, which cannot be merged into a
  -- single value with type 'Bool'. No specification is needed.
  --
  -- >>> runFresh (fresh ()) "a" :: UnionM Bool
  -- {If a@0 False True}
  --
  -- The following example generates @Maybe Bool@s.
  -- There are more than one symbolic constants introduced, and their uniqueness
  -- is ensured. No specification is needed.
  --
  -- >>> runFresh (fresh ()) "a" :: UnionM (Maybe Bool)
  -- {If a@0 Nothing (If a@1 (Just False) (Just True))}
  --
  -- The following example generates lists of symbolic booleans with length 1 to 2.
  --
  -- >>> runFresh (fresh (ListSpec 1 2 ())) "a" :: UnionM [SymBool]
  -- {If a@2 [a@1] [a@0,a@1]}
  --
  -- When multiple symbolic values are generated, there will not be any
  -- identifier collision
  --
  -- >>> runFresh (do; a <- fresh (); b <- fresh (); return (a, b)) "a" :: (UnionM SymBool, UnionM SymBool)
  -- ({a@0},{a@1})
  fresh ::
    (MonadFresh m) =>
    spec ->
    m (UnionM a)
  default fresh ::
    (GenSymSimple spec a) =>
    ( MonadFresh m
    ) =>
    spec ->
    m (UnionM a)
  fresh spec = mrgSingle <$> simpleFresh spec

-- | Generate a symbolic variable wrapped in a Union without the monadic context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
--
-- >>> genSym (ListSpec 1 2 ()) "a" :: UnionM [SymBool]
-- {If a@2 [a@1] [a@0,a@1]}
genSym :: (GenSym spec a) => spec -> FreshIdent -> UnionM a
genSym = runFresh . fresh

-- | Class of types in which symbolic values can be generated with respect to some specification.
--
-- The result will __/not/__ be wrapped in a union-like monad.
--
-- The uniqueness of symbolic constants is managed with the a monadic context.
-- 'Fresh' and 'FreshT' can be useful.
class GenSymSimple spec a where
  -- | Generate a symbolic value given some specification. The uniqueness is ensured.
  --
  -- The following example generates a symbolic boolean. No specification is needed.
  --
  -- >>> runFresh (simpleFresh ()) "a" :: SymBool
  -- a@0
  --
  -- The following code generates list of symbolic boolean with length 2.
  -- As the length is fixed, we don't have to wrap the result in unions.
  --
  -- >>> runFresh (simpleFresh (SimpleListSpec 2 ())) "a" :: [SymBool]
  -- [a@0,a@1]
  simpleFresh ::
    ( MonadFresh m
    ) =>
    spec ->
    m a

-- | Generate a simple symbolic variable wrapped in a Union without the monadic context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
--
-- >>> genSymSimple (SimpleListSpec 2 ()) "a" :: [SymBool]
-- [a@0,a@1]
genSymSimple :: forall spec a. (GenSymSimple spec a) => spec -> FreshIdent -> a
genSymSimple = runFresh . simpleFresh

class GenSymNoSpec a where
  freshNoSpec ::
    ( MonadFresh m
    ) =>
    m (UnionM (a c))

instance GenSymNoSpec U1 where
  freshNoSpec = return $ mrgSingle U1

instance (GenSym () c) => GenSymNoSpec (K1 i c) where
  freshNoSpec = fmap K1 <$> fresh ()

instance (GenSymNoSpec a) => GenSymNoSpec (M1 i c a) where
  freshNoSpec = fmap M1 <$> freshNoSpec

instance
  ( GenSymNoSpec a,
    GenSymNoSpec b,
    forall x. Mergeable (a x),
    forall x. Mergeable (b x)
  ) =>
  GenSymNoSpec (a :+: b)
  where
  freshNoSpec ::
    forall m c.
    ( MonadFresh m
    ) =>
    m (UnionM ((a :+: b) c))
  freshNoSpec = do
    cond :: bool <- simpleFresh ()
    l :: UnionM (a c) <- freshNoSpec
    r :: UnionM (b c) <- freshNoSpec
    return $ mrgIf cond (fmap L1 l) (fmap R1 r)

instance
  (GenSymNoSpec a, GenSymNoSpec b) =>
  GenSymNoSpec (a :*: b)
  where
  freshNoSpec ::
    forall m c.
    ( MonadFresh m
    ) =>
    m (UnionM ((a :*: b) c))
  freshNoSpec = do
    l :: UnionM (a c) <- freshNoSpec
    r :: UnionM (b c) <- freshNoSpec
    return $ do
      l1 <- l
      r1 <- r
      return $ l1 :*: r1

-- | We cannot provide DerivingVia style derivation for 'GenSym', while you can
-- use this 'fresh' implementation to implement 'GenSym' for your own types.
--
-- This 'fresh' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with @()@ as specification,
-- and generate all possible values for a sum type.
--
-- __Note:__ __Never__ use on recursive types.
derivedNoSpecFresh ::
  forall a m.
  ( Generic a,
    GenSymNoSpec (Rep a),
    Mergeable a,
    MonadFresh m
  ) =>
  () ->
  m (UnionM a)
derivedNoSpecFresh _ = merge . fmap to <$> freshNoSpec

class GenSymSimpleNoSpec a where
  simpleFreshNoSpec :: (MonadFresh m) => m (a c)

instance GenSymSimpleNoSpec U1 where
  simpleFreshNoSpec = return U1

instance (GenSymSimple () c) => GenSymSimpleNoSpec (K1 i c) where
  simpleFreshNoSpec = K1 <$> simpleFresh ()

instance (GenSymSimpleNoSpec a) => GenSymSimpleNoSpec (M1 i c a) where
  simpleFreshNoSpec = M1 <$> simpleFreshNoSpec

instance
  (GenSymSimpleNoSpec a, GenSymSimpleNoSpec b) =>
  GenSymSimpleNoSpec (a :*: b)
  where
  simpleFreshNoSpec = do
    l :: a c <- simpleFreshNoSpec
    r :: b c <- simpleFreshNoSpec
    return $ l :*: r

-- | We cannot provide DerivingVia style derivation for 'GenSymSimple', while
-- you can use this 'simpleFresh' implementation to implement 'GenSymSimple' fo
-- your own types.
--
-- This 'simpleFresh' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with '()' as specification.
-- It will not work on sum types.
--
-- __Note:__ __Never__ use on recursive types.
derivedNoSpecSimpleFresh ::
  forall a m.
  ( Generic a,
    GenSymSimpleNoSpec (Rep a),
    MonadFresh m
  ) =>
  () ->
  m a
derivedNoSpecSimpleFresh _ = to <$> simpleFreshNoSpec

class GenSymSameShape a where
  genSymSameShapeFresh ::
    ( MonadFresh m
    ) =>
    a c ->
    m (a c)

instance GenSymSameShape U1 where
  genSymSameShapeFresh _ = return U1

instance (GenSymSimple c c) => GenSymSameShape (K1 i c) where
  genSymSameShapeFresh (K1 c) = K1 <$> simpleFresh c

instance (GenSymSameShape a) => GenSymSameShape (M1 i c a) where
  genSymSameShapeFresh (M1 a) = M1 <$> genSymSameShapeFresh a

instance
  (GenSymSameShape a, GenSymSameShape b) =>
  GenSymSameShape (a :+: b)
  where
  genSymSameShapeFresh (L1 a) = L1 <$> genSymSameShapeFresh a
  genSymSameShapeFresh (R1 a) = R1 <$> genSymSameShapeFresh a

instance
  (GenSymSameShape a, GenSymSameShape b) =>
  GenSymSameShape (a :*: b)
  where
  genSymSameShapeFresh (a :*: b) = do
    l :: a c <- genSymSameShapeFresh a
    r :: b c <- genSymSameShapeFresh b
    return $ l :*: r

-- | We cannot provide DerivingVia style derivation for 'GenSymSimple', while
-- you can use this 'simpleFresh' implementation to implement 'GenSymSimple' fo
-- your own types.
--
-- This 'simpleFresh' implementation is for the types that can be generated with
-- a reference value of the same type.
--
-- For sum types, it will generate the result with the same data constructor.
-- For product types, it will generate the result by generating each field with
-- the corresponding reference value.
--
-- __Note:__ __Can__ be used on recursive types.
derivedSameShapeSimpleFresh ::
  forall a m.
  ( Generic a,
    GenSymSameShape (Rep a),
    MonadFresh m
  ) =>
  a ->
  m a
derivedSameShapeSimpleFresh a = to <$> genSymSameShapeFresh (from a)

-- | Symbolically chooses one of the provided values.
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it
-- is evaluated, and use these variables to conditionally select one of the @n@
-- provided expressions.
--
-- The result will be wrapped in a union-like monad, and also a monad
-- maintaining the 'MonadFresh' context.
--
-- >>> runFresh (chooseFresh [1,2,3]) "a" :: UnionM Integer
-- {If a@0 1 (If a@1 2 3)}
chooseFresh ::
  forall a m.
  ( Mergeable a,
    MonadFresh m
  ) =>
  [a] ->
  m (UnionM a)
chooseFresh [x] = return $ mrgSingle x
chooseFresh (r : rs) = do
  b <- simpleFresh ()
  res <- chooseFresh rs
  return $ mrgIf b (mrgSingle r) res
chooseFresh [] = error "chooseFresh expects at least one value"

-- | A wrapper for `chooseFresh` that executes the `MonadFresh` context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
choose ::
  forall a.
  ( Mergeable a
  ) =>
  [a] ->
  FreshIdent ->
  UnionM a
choose = runFresh . chooseFresh

-- | Symbolically chooses one of the provided values.
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it is evaluated, and use
-- these variables to conditionally select one of the @n@ provided expressions.
--
-- The result will __/not/__ be wrapped in a union-like monad, but will be
-- wrapped in a monad maintaining the 'Fresh' context.
--
-- >>> import Data.Proxy
-- >>> runFresh (chooseSimpleFresh [ssym "b", ssym "c", ssym "d"]) "a" :: SymInteger
-- (ite a@0 b (ite a@1 c d))
chooseSimpleFresh ::
  forall a m.
  ( SimpleMergeable a,
    MonadFresh m
  ) =>
  [a] ->
  m a
chooseSimpleFresh [x] = return x
chooseSimpleFresh (r : rs) = do
  b :: bool <- simpleFresh ()
  res <- chooseSimpleFresh rs
  return $ mrgIte b r res
chooseSimpleFresh [] = error "chooseSimpleFresh expects at least one value"

-- | A wrapper for `chooseSimpleFresh` that executes the `MonadFresh` context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
chooseSimple ::
  forall a.
  ( SimpleMergeable a
  ) =>
  [a] ->
  FreshIdent ->
  a
chooseSimple = runFresh . chooseSimpleFresh

-- | Symbolically chooses one of the provided values wrapped in union-like
-- monads. The procedure creates @n - 1@ fresh symbolic boolean variables every
-- time it is evaluated, and use these variables to conditionally select one of
-- the @n@ provided expressions.
--
-- The result will be wrapped in a union-like monad, and also a monad
-- maintaining the 'Fresh' context.
--
-- >>> let a = runFresh (chooseFresh [1, 2]) "a" :: UnionM Integer
-- >>> let b = runFresh (chooseFresh [2, 3]) "b" :: UnionM Integer
-- >>> runFresh (chooseUnionFresh [a, b]) "c" :: UnionM Integer
-- {If (&& c@0 a@0) 1 (If (|| c@0 b@0) 2 3)}
chooseUnionFresh ::
  forall a m.
  ( Mergeable a,
    MonadFresh m
  ) =>
  [UnionM a] ->
  m (UnionM a)
chooseUnionFresh [x] = return x
chooseUnionFresh (r : rs) = do
  b <- simpleFresh ()
  res <- chooseUnionFresh rs
  return $ mrgIf b r res
chooseUnionFresh [] = error "chooseUnionFresh expects at least one value"

-- | A wrapper for `chooseUnionFresh` that executes the `MonadFresh` context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
chooseUnion ::
  forall a.
  ( Mergeable a
  ) =>
  [UnionM a] ->
  FreshIdent ->
  UnionM a
chooseUnion = runFresh . chooseUnionFresh

#define CONCRETE_GENSYM_SAME_SHAPE(type) \
instance GenSym type type where fresh = return . mrgSingle

#define CONCRETE_GENSYMSIMPLE_SAME_SHAPE(type) \
instance GenSymSimple type type where simpleFresh = return

#define CONCRETE_GENSYM_SAME_SHAPE_BV(type) \
instance (KnownNat n, 1 <= n) => GenSym (type n) (type n) where fresh = return . mrgSingle

#define CONCRETE_GENSYMSIMPLE_SAME_SHAPE_BV(type) \
instance (KnownNat n, 1 <= n) => GenSymSimple (type n) (type n) where simpleFresh = return

#if 1
CONCRETE_GENSYM_SAME_SHAPE(Bool)
CONCRETE_GENSYM_SAME_SHAPE(Integer)
CONCRETE_GENSYM_SAME_SHAPE(Char)
CONCRETE_GENSYM_SAME_SHAPE(Int)
CONCRETE_GENSYM_SAME_SHAPE(Int8)
CONCRETE_GENSYM_SAME_SHAPE(Int16)
CONCRETE_GENSYM_SAME_SHAPE(Int32)
CONCRETE_GENSYM_SAME_SHAPE(Int64)
CONCRETE_GENSYM_SAME_SHAPE(Word)
CONCRETE_GENSYM_SAME_SHAPE(Word8)
CONCRETE_GENSYM_SAME_SHAPE(Word16)
CONCRETE_GENSYM_SAME_SHAPE(Word32)
CONCRETE_GENSYM_SAME_SHAPE(Word64)
CONCRETE_GENSYM_SAME_SHAPE(SomeWordN)
CONCRETE_GENSYM_SAME_SHAPE(SomeIntN)
CONCRETE_GENSYM_SAME_SHAPE(B.ByteString)
CONCRETE_GENSYM_SAME_SHAPE(T.Text)
CONCRETE_GENSYM_SAME_SHAPE_BV(WordN)
CONCRETE_GENSYM_SAME_SHAPE_BV(IntN)

CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Bool)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Integer)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Char)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Int)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Int8)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Int16)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Int32)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Int64)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Word)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Word8)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Word16)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Word32)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(Word64)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(SomeWordN)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(SomeIntN)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(B.ByteString)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(T.Text)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE_BV(WordN)
CONCRETE_GENSYMSIMPLE_SAME_SHAPE_BV(IntN)
#endif

-- Bool
instance GenSym () Bool where
  fresh = derivedNoSpecFresh

-- Enums

-- | Specification for enum values with upper bound (exclusive). The result would chosen from [0 .. upperbound].
--
-- >>> runFresh (fresh (EnumGenUpperBound @Integer 4)) "c" :: UnionM Integer
-- {If c@0 0 (If c@1 1 (If c@2 2 3))}
newtype EnumGenUpperBound a = EnumGenUpperBound a

instance (Enum v, Mergeable v) => GenSym (EnumGenUpperBound v) v where
  fresh (EnumGenUpperBound u) = chooseFresh (toEnum <$> [0 .. fromEnum u - 1])

-- | Specification for numbers with lower bound (inclusive) and upper bound (exclusive)
--
-- >>> runFresh (fresh (EnumGenBound @Integer 0 4)) "c" :: UnionM Integer
-- {If c@0 0 (If c@1 1 (If c@2 2 3))}
data EnumGenBound a = EnumGenBound a a

instance (Enum v, Mergeable v) => GenSym (EnumGenBound v) v where
  fresh (EnumGenBound l u) = chooseFresh (toEnum <$> [fromEnum l .. fromEnum u - 1])

-- Either
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b
  ) =>
  GenSym (Either aspec bspec) (Either a b)
  where
  fresh (Left aspec) = (merge . fmap Left) <$> fresh aspec
  fresh (Right bspec) = (merge . fmap Right) <$> fresh bspec

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b
  ) =>
  GenSymSimple (Either aspec bspec) (Either a b)
  where
  simpleFresh (Left a) = Left <$> simpleFresh a
  simpleFresh (Right b) = Right <$> simpleFresh b

instance
  (GenSym () a, Mergeable a, GenSym () b, Mergeable b) =>
  GenSym () (Either a b)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b
  ) =>
  GenSym (aspec, bspec) (Either a b)
  where
  fresh (aspec, bspec) = do
    l :: UnionM a <- fresh aspec
    r :: UnionM b <- fresh bspec
    chooseUnionFresh [Left <$> l, Right <$> r]

-- Maybe
instance
  {-# OVERLAPPING #-}
  (GenSym aspec a, Mergeable a) =>
  GenSym (Maybe aspec) (Maybe a)
  where
  fresh Nothing = return $ mrgSingle Nothing
  fresh (Just aspec) = (merge . fmap Just) <$> fresh aspec

instance
  (GenSymSimple aspec a) =>
  GenSymSimple (Maybe aspec) (Maybe a)
  where
  simpleFresh Nothing = return Nothing
  simpleFresh (Just aspec) = Just <$> simpleFresh aspec

instance
  {-# OVERLAPPABLE #-}
  (GenSym aspec a, Mergeable a) =>
  GenSym aspec (Maybe a)
  where
  fresh aspec = do
    cond <- simpleFresh ()
    a :: UnionM a <- fresh aspec
    return $ mrgIf cond (mrgSingle Nothing) (Just <$> a)

-- List
instance
  (GenSym () a, Mergeable a) =>
  GenSym Integer [a]
  where
  fresh v = do
    l <- gl v
    let xs = reverse $ scanr (:) [] l
    chooseUnionFresh $ merge . sequence <$> xs
    where
      gl :: (MonadFresh m) => Integer -> m [UnionM a]
      gl v1
        | v1 <= 0 = return []
        | otherwise = do
            l <- fresh ()
            r <- gl (v1 - 1)
            return $ l : r

-- | Specification for list generation.
--
-- >>> runFresh (fresh (ListSpec 0 2 ())) "c" :: UnionM [SymBool]
-- {If c@2 [] (If c@3 [c@1] [c@0,c@1])}
--
-- >>> runFresh (fresh (ListSpec 0 2 (SimpleListSpec 1 ()))) "c" :: UnionM [[SymBool]]
-- {If c@2 [] (If c@3 [[c@1]] [[c@0],[c@1]])}
data ListSpec spec = ListSpec
  { -- | The minimum length of the generated lists
    genListMinLength :: Int,
    -- | The maximum length of the generated lists
    genListMaxLength :: Int,
    -- | Each element in the lists will be generated with the sub-specification
    genListSubSpec :: spec
  }
  deriving (Show)

instance
  (GenSym spec a, Mergeable a) =>
  GenSym (ListSpec spec) [a]
  where
  fresh (ListSpec minLen maxLen subSpec) =
    if minLen < 0 || maxLen < 0 || minLen >= maxLen
      then error $ "Bad lengths: " ++ show (minLen, maxLen)
      else do
        l <- gl maxLen
        let xs = drop minLen $ reverse $ scanr (:) [] l
        chooseUnionFresh $ merge . sequence <$> xs
    where
      gl :: (MonadFresh m) => Int -> m [UnionM a]
      gl currLen
        | currLen <= 0 = return []
        | otherwise = do
            l <- fresh subSpec
            r <- gl (currLen - 1)
            return $ l : r

instance
  (GenSym a a, Mergeable a) =>
  GenSym [a] [a]
  where
  fresh l = do
    r :: [UnionM a] <- traverse fresh l
    return $ merge $ sequence r

instance
  (GenSymSimple a a) =>
  GenSymSimple [a] [a]
  where
  simpleFresh = derivedSameShapeSimpleFresh

-- | Specification for list generation of a specific length.
--
-- >>> runFresh (simpleFresh (SimpleListSpec 2 ())) "c" :: [SymBool]
-- [c@0,c@1]
data SimpleListSpec spec = SimpleListSpec
  { -- | The length of the generated list
    genSimpleListLength :: Int,
    -- | Each element in the list will be generated with the sub-specification
    genSimpleListSubSpec :: spec
  }
  deriving (Show)

instance
  (GenSym spec a, Mergeable a) =>
  GenSym (SimpleListSpec spec) [a]
  where
  fresh (SimpleListSpec len subSpec) =
    if len < 0
      then error $ "Bad lengths: " ++ show len
      else do
        merge . sequence <$> gl len
    where
      gl :: (MonadFresh m) => Int -> m [UnionM a]
      gl currLen
        | currLen <= 0 = return []
        | otherwise = do
            l <- fresh subSpec
            r <- gl (currLen - 1)
            return $ l : r

instance
  (GenSymSimple spec a) =>
  GenSymSimple (SimpleListSpec spec) [a]
  where
  simpleFresh (SimpleListSpec len subSpec) =
    if len < 0
      then error $ "Bad lengths: " ++ show len
      else do
        gl len
    where
      gl :: (MonadFresh m) => Int -> m [a]
      gl currLen
        | currLen <= 0 = return []
        | otherwise = do
            l <- simpleFresh subSpec
            r <- gl (currLen - 1)
            return $ l : r

-- ()
instance GenSym () ()

instance GenSymSimple () () where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,)
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b
  ) =>
  GenSym (aspec, bspec) (a, b)
  where
  fresh (aspec, bspec) = do
    a1 <- fresh aspec
    b1 <- fresh bspec
    return $ do
      ax <- a1
      bx <- b1
      mrgSingle (ax, bx)

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b
  ) =>
  GenSymSimple (aspec, bspec) (a, b)
  where
  simpleFresh (aspec, bspec) = do
    (,)
      <$> simpleFresh aspec
      <*> simpleFresh bspec

instance
  (GenSym () a, Mergeable a, GenSym () b, Mergeable b) =>
  GenSym () (a, b)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b
  ) =>
  GenSymSimple () (a, b)
  where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,,)
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b,
    GenSym cspec c,
    Mergeable c
  ) =>
  GenSym (aspec, bspec, cspec) (a, b, c)
  where
  fresh (aspec, bspec, cspec) = do
    a1 <- fresh aspec
    b1 <- fresh bspec
    c1 <- fresh cspec
    return $ do
      ax <- a1
      bx <- b1
      cx <- c1
      mrgSingle (ax, bx, cx)

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b,
    GenSymSimple cspec c
  ) =>
  GenSymSimple (aspec, bspec, cspec) (a, b, c)
  where
  simpleFresh (aspec, bspec, cspec) = do
    (,,)
      <$> simpleFresh aspec
      <*> simpleFresh bspec
      <*> simpleFresh cspec

instance
  ( GenSym () a,
    Mergeable a,
    GenSym () b,
    Mergeable b,
    GenSym () c,
    Mergeable c
  ) =>
  GenSym () (a, b, c)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c
  ) =>
  GenSymSimple () (a, b, c)
  where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,,,)
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b,
    GenSym cspec c,
    Mergeable c,
    GenSym dspec d,
    Mergeable d
  ) =>
  GenSym (aspec, bspec, cspec, dspec) (a, b, c, d)
  where
  fresh (aspec, bspec, cspec, dspec) = do
    a1 <- fresh aspec
    b1 <- fresh bspec
    c1 <- fresh cspec
    d1 <- fresh dspec
    return $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      mrgSingle (ax, bx, cx, dx)

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b,
    GenSymSimple cspec c,
    GenSymSimple dspec d
  ) =>
  GenSymSimple (aspec, bspec, cspec, dspec) (a, b, c, d)
  where
  simpleFresh (aspec, bspec, cspec, dspec) = do
    (,,,)
      <$> simpleFresh aspec
      <*> simpleFresh bspec
      <*> simpleFresh cspec
      <*> simpleFresh dspec

instance
  ( GenSym () a,
    Mergeable a,
    GenSym () b,
    Mergeable b,
    GenSym () c,
    Mergeable c,
    GenSym () d,
    Mergeable d
  ) =>
  GenSym () (a, b, c, d)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c,
    GenSymSimple () d
  ) =>
  GenSymSimple () (a, b, c, d)
  where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,,,,)
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b,
    GenSym cspec c,
    Mergeable c,
    GenSym dspec d,
    Mergeable d,
    GenSym espec e,
    Mergeable e
  ) =>
  GenSym (aspec, bspec, cspec, dspec, espec) (a, b, c, d, e)
  where
  fresh (aspec, bspec, cspec, dspec, espec) = do
    a1 <- fresh aspec
    b1 <- fresh bspec
    c1 <- fresh cspec
    d1 <- fresh dspec
    e1 <- fresh espec
    return $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      mrgSingle (ax, bx, cx, dx, ex)

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b,
    GenSymSimple cspec c,
    GenSymSimple dspec d,
    GenSymSimple espec e
  ) =>
  GenSymSimple (aspec, bspec, cspec, dspec, espec) (a, b, c, d, e)
  where
  simpleFresh (aspec, bspec, cspec, dspec, espec) = do
    (,,,,)
      <$> simpleFresh aspec
      <*> simpleFresh bspec
      <*> simpleFresh cspec
      <*> simpleFresh dspec
      <*> simpleFresh espec

instance
  ( GenSym () a,
    Mergeable a,
    GenSym () b,
    Mergeable b,
    GenSym () c,
    Mergeable c,
    GenSym () d,
    Mergeable d,
    GenSym () e,
    Mergeable e
  ) =>
  GenSym () (a, b, c, d, e)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c,
    GenSymSimple () d,
    GenSymSimple () e
  ) =>
  GenSymSimple () (a, b, c, d, e)
  where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,,,,,)
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b,
    GenSym cspec c,
    Mergeable c,
    GenSym dspec d,
    Mergeable d,
    GenSym espec e,
    Mergeable e,
    GenSym fspec f,
    Mergeable f
  ) =>
  GenSym (aspec, bspec, cspec, dspec, espec, fspec) (a, b, c, d, e, f)
  where
  fresh (aspec, bspec, cspec, dspec, espec, fspec) = do
    a1 <- fresh aspec
    b1 <- fresh bspec
    c1 <- fresh cspec
    d1 <- fresh dspec
    e1 <- fresh espec
    f1 <- fresh fspec
    return $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      fx <- f1
      mrgSingle (ax, bx, cx, dx, ex, fx)

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b,
    GenSymSimple cspec c,
    GenSymSimple dspec d,
    GenSymSimple espec e,
    GenSymSimple fspec f
  ) =>
  GenSymSimple (aspec, bspec, cspec, dspec, espec, fspec) (a, b, c, d, e, f)
  where
  simpleFresh (aspec, bspec, cspec, dspec, espec, fspec) = do
    (,,,,,)
      <$> simpleFresh aspec
      <*> simpleFresh bspec
      <*> simpleFresh cspec
      <*> simpleFresh dspec
      <*> simpleFresh espec
      <*> simpleFresh fspec

instance
  ( GenSym () a,
    Mergeable a,
    GenSym () b,
    Mergeable b,
    GenSym () c,
    Mergeable c,
    GenSym () d,
    Mergeable d,
    GenSym () e,
    Mergeable e,
    GenSym () f,
    Mergeable f
  ) =>
  GenSym () (a, b, c, d, e, f)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c,
    GenSymSimple () d,
    GenSymSimple () e,
    GenSymSimple () f
  ) =>
  GenSymSimple () (a, b, c, d, e, f)
  where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,,,,,,)
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b,
    GenSym cspec c,
    Mergeable c,
    GenSym dspec d,
    Mergeable d,
    GenSym espec e,
    Mergeable e,
    GenSym fspec f,
    Mergeable f,
    GenSym gspec g,
    Mergeable g
  ) =>
  GenSym (aspec, bspec, cspec, dspec, espec, fspec, gspec) (a, b, c, d, e, f, g)
  where
  fresh (aspec, bspec, cspec, dspec, espec, fspec, gspec) = do
    a1 <- fresh aspec
    b1 <- fresh bspec
    c1 <- fresh cspec
    d1 <- fresh dspec
    e1 <- fresh espec
    f1 <- fresh fspec
    g1 <- fresh gspec
    return $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      fx <- f1
      gx <- g1
      mrgSingle (ax, bx, cx, dx, ex, fx, gx)

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b,
    GenSymSimple cspec c,
    GenSymSimple dspec d,
    GenSymSimple espec e,
    GenSymSimple fspec f,
    GenSymSimple gspec g
  ) =>
  GenSymSimple (aspec, bspec, cspec, dspec, espec, fspec, gspec) (a, b, c, d, e, f, g)
  where
  simpleFresh (aspec, bspec, cspec, dspec, espec, fspec, gspec) = do
    (,,,,,,)
      <$> simpleFresh aspec
      <*> simpleFresh bspec
      <*> simpleFresh cspec
      <*> simpleFresh dspec
      <*> simpleFresh espec
      <*> simpleFresh fspec
      <*> simpleFresh gspec

instance
  ( GenSym () a,
    Mergeable a,
    GenSym () b,
    Mergeable b,
    GenSym () c,
    Mergeable c,
    GenSym () d,
    Mergeable d,
    GenSym () e,
    Mergeable e,
    GenSym () f,
    Mergeable f,
    GenSym () g,
    Mergeable g
  ) =>
  GenSym () (a, b, c, d, e, f, g)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c,
    GenSymSimple () d,
    GenSymSimple () e,
    GenSymSimple () f,
    GenSymSimple () g
  ) =>
  GenSymSimple () (a, b, c, d, e, f, g)
  where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,,,,,,,)
instance
  ( GenSym aspec a,
    Mergeable a,
    GenSym bspec b,
    Mergeable b,
    GenSym cspec c,
    Mergeable c,
    GenSym dspec d,
    Mergeable d,
    GenSym espec e,
    Mergeable e,
    GenSym fspec f,
    Mergeable f,
    GenSym gspec g,
    Mergeable g,
    GenSym hspec h,
    Mergeable h
  ) =>
  GenSym (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) (a, b, c, d, e, f, g, h)
  where
  fresh (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) = do
    a1 <- fresh aspec
    b1 <- fresh bspec
    c1 <- fresh cspec
    d1 <- fresh dspec
    e1 <- fresh espec
    f1 <- fresh fspec
    g1 <- fresh gspec
    h1 <- fresh hspec
    return $ do
      ax <- a1
      bx <- b1
      cx <- c1
      dx <- d1
      ex <- e1
      fx <- f1
      gx <- g1
      hx <- h1
      mrgSingle (ax, bx, cx, dx, ex, fx, gx, hx)

instance
  ( GenSymSimple aspec a,
    GenSymSimple bspec b,
    GenSymSimple cspec c,
    GenSymSimple dspec d,
    GenSymSimple espec e,
    GenSymSimple fspec f,
    GenSymSimple gspec g,
    GenSymSimple hspec h
  ) =>
  GenSymSimple (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) (a, b, c, d, e, f, g, h)
  where
  simpleFresh (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) = do
    (,,,,,,,)
      <$> simpleFresh aspec
      <*> simpleFresh bspec
      <*> simpleFresh cspec
      <*> simpleFresh dspec
      <*> simpleFresh espec
      <*> simpleFresh fspec
      <*> simpleFresh gspec
      <*> simpleFresh hspec

instance
  ( GenSym () a,
    Mergeable a,
    GenSym () b,
    Mergeable b,
    GenSym () c,
    Mergeable c,
    GenSym () d,
    Mergeable d,
    GenSym () e,
    Mergeable e,
    GenSym () f,
    Mergeable f,
    GenSym () g,
    Mergeable g,
    GenSym () h,
    Mergeable h
  ) =>
  GenSym () (a, b, c, d, e, f, g, h)
  where
  fresh = derivedNoSpecFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c,
    GenSymSimple () d,
    GenSymSimple () e,
    GenSymSimple () f,
    GenSymSimple () g,
    GenSymSimple () h
  ) =>
  GenSymSimple () (a, b, c, d, e, f, g, h)
  where
  simpleFresh = derivedNoSpecSimpleFresh

-- MaybeT
instance
  {-# OVERLAPPABLE #-}
  ( GenSym spec (m (Maybe a)),
    Mergeable1 m,
    Mergeable a
  ) =>
  GenSym spec (MaybeT m a)
  where
  fresh v = do
    x <- fresh v
    return $ merge . fmap MaybeT $ x

instance
  {-# OVERLAPPABLE #-}
  ( GenSymSimple spec (m (Maybe a))
  ) =>
  GenSymSimple spec (MaybeT m a)
  where
  simpleFresh v = MaybeT <$> simpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (Maybe a)) (m (Maybe a))
  ) =>
  GenSymSimple (MaybeT m a) (MaybeT m a)
  where
  simpleFresh (MaybeT v) = MaybeT <$> simpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (Maybe a)) (m (Maybe a)),
    Mergeable1 m,
    Mergeable a
  ) =>
  GenSym (MaybeT m a) (MaybeT m a)

-- ExceptT
instance
  {-# OVERLAPPABLE #-}
  ( GenSym spec (m (Either a b)),
    Mergeable1 m,
    Mergeable a,
    Mergeable b
  ) =>
  GenSym spec (ExceptT a m b)
  where
  fresh v = do
    x <- fresh v
    return $ merge . fmap ExceptT $ x

instance
  {-# OVERLAPPABLE #-}
  ( GenSymSimple spec (m (Either a b))
  ) =>
  GenSymSimple spec (ExceptT a m b)
  where
  simpleFresh v = ExceptT <$> simpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (Either e a)) (m (Either e a))
  ) =>
  GenSymSimple (ExceptT e m a) (ExceptT e m a)
  where
  simpleFresh (ExceptT v) = ExceptT <$> simpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (Either e a)) (m (Either e a)),
    Mergeable1 m,
    Mergeable e,
    Mergeable a
  ) =>
  GenSym (ExceptT e m a) (ExceptT e m a)

#define GENSYM_SIMPLE(symtype) \
instance GenSym symtype symtype
#define GENSYM_SIMPLE_SIMPLE(symtype) \
instance GenSymSimple symtype symtype where \
  simpleFresh _ = simpleFresh ()
#define GENSYM_UNIT_SIMPLE(symtype) \
instance GenSym () symtype where \
  fresh _ = mrgSingle <$> simpleFresh ()
#define GENSYM_UNIT_SIMPLE_SIMPLE(symtype) \
instance GenSymSimple () symtype where \
  simpleFresh _ = do; \
    ident <- getFreshIdent; \
    FreshIndex i <- nextFreshIndex; \
    case ident of; \
      FreshIdent s -> return $ isym s i; \
      FreshIdentWithInfo s info -> return $ iinfosym s i info

#define GENSYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => GenSym (symtype n) (symtype n)
#define GENSYM_SIMPLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => GenSymSimple (symtype n) (symtype n) where \
  simpleFresh _ = simpleFresh ()
#define GENSYM_UNIT_BV(symtype) \
instance (KnownNat n, 1 <= n) => GenSym () (symtype n) where \
  fresh _ = mrgSingle <$> simpleFresh ()
#define GENSYM_UNIT_SIMPLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => GenSymSimple () (symtype n) where \
  simpleFresh _ = do; \
    ident <- getFreshIdent; \
    FreshIndex i <- nextFreshIndex; \
    case ident of; \
      FreshIdent s -> return $ isym s i; \
      FreshIdentWithInfo s info -> return $ iinfosym s i info

#define GENSYM_BV_SOME(symtype) \
instance GenSym symtype symtype
#define GENSYM_SIMPLE_BV_SOME(symtype) \
instance GenSymSimple symtype symtype where \
  simpleFresh (symtype v) = simpleFresh v
#define GENSYM_N_BV_SOME(symtype) \
instance (KnownNat n, 1 <= n) => GenSym (p n) symtype where \
  fresh p = mrgSingle <$> simpleFresh p
#define GENSYM_N_SIMPLE_BV_SOME(symtype, origtype) \
instance (KnownNat n, 1 <= n) => GenSymSimple (p n) symtype where \
  simpleFresh _ = do; \
    i :: origtype n <- simpleFresh (); \
    return $ symtype i
#define GENSYM_N_INT_BV_SOME(symtype) \
instance GenSym Int symtype where \
  fresh p = mrgSingle <$> simpleFresh p
#define GENSYM_N_INT_SIMPLE_BV_SOME(symtype, origtype) \
instance GenSymSimple Int symtype where \
  simpleFresh i = if i > 0 then f (Proxy @0) else \
    error "Can only generate bit vectors with positive bit size" \
    where \
      f :: forall p (n :: Nat) m. (MonadFresh m) => p n -> m symtype; \
      f _ = case (unsafeKnownProof @n (fromIntegral i), unsafeLeqProof @1 @n) of \
        (KnownProof, LeqProof) -> do \
        v :: origtype n <- simpleFresh (); \
        return $ symtype v; \

#define GENSYM_FUN(op) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => GenSym (sa op sb) (sa op sb)
#define GENSYM_SIMPLE_FUN(op) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => GenSymSimple (sa op sb) (sa op sb) where \
  simpleFresh _ = simpleFresh ()
#define GENSYM_UNIT_FUN(op) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => GenSym () (sa op sb) where \
  fresh _ = mrgSingle <$> simpleFresh ()
#define GENSYM_UNIT_SIMPLE_FUN(op) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => GenSymSimple () (sa op sb) where \
  simpleFresh _ = do; \
    ident <- getFreshIdent; \
    FreshIndex i <- nextFreshIndex; \
    case ident of; \
      FreshIdent s -> return $ isym s i; \
      FreshIdentWithInfo s info -> return $ iinfosym s i info

#if 1
GENSYM_SIMPLE(SymBool)
GENSYM_SIMPLE_SIMPLE(SymBool)
GENSYM_UNIT_SIMPLE(SymBool)
GENSYM_UNIT_SIMPLE_SIMPLE(SymBool)
GENSYM_SIMPLE(SymInteger)
GENSYM_SIMPLE_SIMPLE(SymInteger)
GENSYM_UNIT_SIMPLE(SymInteger)
GENSYM_UNIT_SIMPLE_SIMPLE(SymInteger)

GENSYM_BV(SymIntN)
GENSYM_SIMPLE_BV(SymIntN)
GENSYM_UNIT_BV(SymIntN)
GENSYM_UNIT_SIMPLE_BV(SymIntN)
GENSYM_BV(SymWordN)
GENSYM_SIMPLE_BV(SymWordN)
GENSYM_UNIT_BV(SymWordN)
GENSYM_UNIT_SIMPLE_BV(SymWordN)

GENSYM_BV_SOME(SomeSymIntN)
GENSYM_SIMPLE_BV_SOME(SomeSymIntN)
GENSYM_N_BV_SOME(SomeSymIntN)
GENSYM_N_SIMPLE_BV_SOME(SomeSymIntN, SymIntN)
GENSYM_N_INT_BV_SOME(SomeSymIntN)
GENSYM_N_INT_SIMPLE_BV_SOME(SomeSymIntN, SymIntN)
GENSYM_BV_SOME(SomeSymWordN)
GENSYM_SIMPLE_BV_SOME(SomeSymWordN)
GENSYM_N_BV_SOME(SomeSymWordN)
GENSYM_N_SIMPLE_BV_SOME(SomeSymWordN, SymWordN)
GENSYM_N_INT_BV_SOME(SomeSymWordN)
GENSYM_N_INT_SIMPLE_BV_SOME(SomeSymWordN, SymWordN)

GENSYM_FUN(=~>)
GENSYM_SIMPLE_FUN(=~>)
GENSYM_UNIT_FUN(=~>)
GENSYM_UNIT_SIMPLE_FUN(=~>)
GENSYM_FUN(-~>)
GENSYM_SIMPLE_FUN(-~>)
GENSYM_UNIT_FUN(-~>)
GENSYM_UNIT_SIMPLE_FUN(-~>)
#endif

instance (GenSym spec a, Mergeable a) => GenSym spec (UnionM a)

instance (GenSym spec a) => GenSymSimple spec (UnionM a) where
  simpleFresh spec = do
    res <- fresh spec
    if not (isMerged res) then error "Not merged" else return res

instance
  (GenSym a a, Mergeable a) =>
  GenSym (UnionM a) a
  where
  fresh spec = go (underlyingUnion $ merge spec)
    where
      go (UnionSingle x) = fresh x
      go (UnionIf _ _ _ t f) = mrgIf <$> simpleFresh () <*> go t <*> go f
