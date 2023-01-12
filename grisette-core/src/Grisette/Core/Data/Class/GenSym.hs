{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.GenSym
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Indices and identifiers for fresh symbolic value generation
    FreshIndex (..),
    FreshIdent (..),
    name,
    nameWithInfo,

    -- * Monad for fresh symbolic value generation
    MonadFresh (..),
    FreshT,
    Fresh,
    runFreshT,
    runFresh,

    -- * Symbolic value generation
    GenSym (..),
    GenSymSimple (..),
    genSym,
    genSymSimple,
    derivedNoSpecFresh,
    derivedNoSpecSimpleFresh,
    derivedSameShapeSimpleFresh,

    -- * Symbolic choices
    gchooseFresh,
    gchooseSimpleFresh,
    gchooseUnionFresh,
    gchoose,
    gchooseSimple,
    gchooseUnion,

    -- * Some common GenSym specifications
    ListSpec (..),
    SimpleListSpec (..),
    EnumGenBound (..),
    EnumGenUpperBound (..),
  )
where

import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Signatures
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import qualified Data.ByteString as B
import Data.Hashable
import Data.Int
import Data.String
import Data.Typeable
import Data.Word
import Generics.Deriving hiding (index)
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Language.Haskell.TH.Syntax hiding (lift)

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

instance (SymBoolOp bool) => GMergeable bool FreshIndex where
  grootStrategy = SimpleStrategy $ \_ t f -> max t f

instance (SymBoolOp bool) => GSimpleMergeable bool FreshIndex where
  gmrgIte _ = max

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
  FreshIdent :: String -> FreshIdent
  FreshIdentWithInfo :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> FreshIdent

instance Show FreshIdent where
  show (FreshIdent i) = i
  show (FreshIdentWithInfo s i) = s ++ ":" ++ show i

instance IsString FreshIdent where
  fromString = name

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
name :: String -> FreshIdent
name = FreshIdent

-- | Identifier with extra information.
-- The same name with the same information
-- refers to the same symbolic variable in the whole program.
--
-- The user may need to use unique names or additional information to avoid
-- unintentional identifier collision.
nameWithInfo :: forall a. (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> FreshIdent
nameWithInfo = FreshIdentWithInfo

-- | Monad class for fresh symbolic value generation.
--
-- The monad should be a reader monad for the 'FreshIdent' and a state monad for
-- the 'FreshIndex'.
class Monad m => MonadFresh m where
  -- | Increase the index by one and return the new index.
  nextFreshIndex :: m FreshIndex

  -- | Get the identifier.
  getFreshIdent :: m FreshIdent

-- | A symbolic generation monad transformer.
-- It is a reader monad transformer for identifiers and
-- a state monad transformer for indices.
--
-- Each time a fresh symbolic variable is generated, the index should be increased.
newtype FreshT m a = FreshT {runFreshT' :: FreshIdent -> FreshIndex -> m (a, FreshIndex)}

instance
  (SymBoolOp bool, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (FreshT m a)
  where
  grootStrategy =
    gwrapStrategy (liftGRootStrategy (liftGRootStrategy grootStrategy1)) FreshT runFreshT'

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (FreshT m) where
  liftGRootStrategy m =
    gwrapStrategy
      (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy (liftGRootStrategy2 m grootStrategy))))
      FreshT
      runFreshT'

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a) =>
  GSimpleMergeable bool (FreshT m a)
  where
  gmrgIte = mrgIf

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable1 bool (FreshT m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GUnionLike bool (FreshT m)
  where
  mergeWithStrategy s (FreshT f) =
    FreshT $ \ident index -> mergeWithStrategy (liftGRootStrategy2 s grootStrategy) $ f ident index
  mrgIfWithStrategy s cond (FreshT t) (FreshT f) =
    FreshT $ \ident index -> mrgIfWithStrategy (liftGRootStrategy2 s grootStrategy) cond (t ident index) (f ident index)
  single x = FreshT $ \_ i -> single (x, i)
  unionIf cond (FreshT t) (FreshT f) =
    FreshT $ \ident index -> unionIf cond (t ident index) (f ident index)

-- | Run the symbolic generation with the given identifier and 0 as the initial index.
runFreshT :: (Monad m) => FreshT m a -> FreshIdent -> m a
runFreshT m ident = fst <$> runFreshT' m ident (FreshIndex 0)

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
    runFreshT' (f a) ident idx'

instance MonadTrans FreshT where
  lift x = FreshT $ \_ index -> (,index) <$> x

liftFreshTCache :: (Functor m) => Catch e m (a, FreshIndex) -> Catch e (FreshT m) a
liftFreshTCache catchE (FreshT m) h =
  FreshT $ \ident index -> m ident index `catchE` \e -> runFreshT' (h e) ident index

instance (MonadError e m) => MonadError e (FreshT m) where
  throwError = lift . throwError
  catchError = liftFreshTCache catchError

-- | 'FreshT' specialized with Identity.
type Fresh = FreshT Identity

-- | Run the symbolic generation with the given identifier and 0 as the initial index.
runFresh :: Fresh a -> FreshIdent -> a
runFresh m ident = runIdentity $ runFreshT m ident

instance Monad m => MonadFresh (FreshT m) where
  nextFreshIndex = FreshT $ \_ idx -> return (idx, idx + 1)
  getFreshIdent = FreshT $ curry return

-- | Class of types in which symbolic values can be generated with respect to some specification.
--
-- The result will be wrapped in a union-like monad.
-- This ensures that we can generate those types with complex merging rules.
--
-- The uniqueness of symbolic constants is managed with the a monadic context.
-- 'Fresh' and 'FreshT' can be useful.
--
-- __Note:__ The @bool@ type is the symbolic boolean type to use. It should
-- be an instance of `SymBoolOp`.
class (SymBoolOp bool, GMergeable bool a) => GenSym bool spec a where
  -- | Generate a symbolic value given some specification. Within a single
  -- `MonadFresh` context, calls to `fresh` would generate unique symbolic
  -- constants.
  --
  -- The following example generates a symbolic boolean. No specification is
  -- needed.
  --
  -- >>> runFresh (fresh ()) "a" :: UnionM SymBool
  -- UMrg (Single a@0)
  --
  -- The following example generates booleans, which cannot be merged into a
  -- single value with type 'Bool'. No specification is needed.
  --
  -- >>> runFresh (fresh ()) "a" :: UnionM Bool
  -- UMrg (If a@0 (Single False) (Single True))
  --
  -- The following example generates @Maybe Bool@s.
  -- There are more than one symbolic constants introduced, and their uniqueness
  -- is ensured. No specification is needed.
  --
  -- >>> runFresh (fresh ()) "a" :: UnionM (Maybe Bool)
  -- UMrg (If a@0 (Single Nothing) (If a@1 (Single (Just False)) (Single (Just True))))
  --
  -- The following example generates lists of symbolic booleans with length 1 to 2.
  --
  -- >>> runFresh (fresh (ListSpec 1 2 ())) "a" :: UnionM [SymBool]
  -- UMrg (If a@2 (Single [a@1]) (Single [a@0,a@1]))
  --
  -- When multiple symbolic values are generated, there will not be any
  -- identifier collision
  --
  -- >>> runFresh (do; a <- fresh (); b <- fresh (); return (a, b)) "a" :: (UnionM SymBool, UnionM SymBool)
  -- (UMrg (Single a@0),UMrg (Single a@1))
  fresh ::
    ( MonadFresh m,
      GMonadUnion bool u
    ) =>
    spec ->
    m (u a)
  default fresh ::
    (GenSymSimple spec a) =>
    ( MonadFresh m,
      GMonadUnion bool u
    ) =>
    spec ->
    m (u a)
  fresh spec = mrgSingle <$> simpleFresh spec

-- | Generate a symbolic variable wrapped in a Union without the monadic context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
--
-- >>> genSym (ListSpec 1 2 ()) "a" :: UnionM [SymBool]
-- UMrg (If a@2 (Single [a@1]) (Single [a@0,a@1]))
genSym :: (GenSym bool spec a, GMonadUnion bool u) => spec -> FreshIdent -> u a
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

class GenSymNoSpec bool a where
  freshNoSpec ::
    ( MonadFresh m,
      GMonadUnion bool u
    ) =>
    m (u (a c))

instance (SymBoolOp bool) => GenSymNoSpec bool U1 where
  freshNoSpec = return $ mrgSingle U1

instance (SymBoolOp bool, GenSym bool () c) => GenSymNoSpec bool (K1 i c) where
  freshNoSpec = fmap K1 <$> fresh ()

instance (SymBoolOp bool, GenSymNoSpec bool a) => GenSymNoSpec bool (M1 i c a) where
  freshNoSpec = fmap M1 <$> freshNoSpec

instance
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSymNoSpec bool a,
    GenSymNoSpec bool b,
    forall x. GMergeable bool (a x),
    forall x. GMergeable bool (b x)
  ) =>
  GenSymNoSpec bool (a :+: b)
  where
  freshNoSpec ::
    forall m u c.
    ( MonadFresh m,
      GMonadUnion bool u
    ) =>
    m (u ((a :+: b) c))
  freshNoSpec = do
    cond :: bool <- simpleFresh ()
    l :: u (a c) <- freshNoSpec
    r :: u (b c) <- freshNoSpec
    return $ mrgIf cond (fmap L1 l) (fmap R1 r)

instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymNoSpec bool a, GenSymNoSpec bool b) =>
  GenSymNoSpec bool (a :*: b)
  where
  freshNoSpec ::
    forall m u c.
    ( MonadFresh m,
      GMonadUnion bool u
    ) =>
    m (u ((a :*: b) c))
  freshNoSpec = do
    l :: u (a c) <- freshNoSpec
    r :: u (b c) <- freshNoSpec
    return $ do
      l1 <- l
      r1 <- r
      return $ l1 :*: r1

-- | We cannot provide DerivingVia style derivation for 'GenSym', while you can
-- use this 'fresh' implementation to implement 'GenSym' for your own types.
--
-- This 'fresh' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with '()' as specification,
-- and generate all possible values for a sum type.
--
-- __Note:__ __Never__ use on recursive types.
derivedNoSpecFresh ::
  forall bool a m u.
  ( Generic a,
    SymBoolOp bool,
    GenSymNoSpec bool (Rep a),
    GMergeable bool a,
    MonadFresh m,
    GMonadUnion bool u
  ) =>
  () ->
  m (u a)
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
-- >>> runFresh (gchooseFresh [1,2,3]) "a" :: UnionM Integer
-- UMrg (If a@0 (Single 1) (If a@1 (Single 2) (Single 3)))
gchooseFresh ::
  forall bool a m u.
  ( SymBoolOp bool,
    GMergeable bool a,
    GenSymSimple () bool,
    MonadFresh m,
    GMonadUnion bool u
  ) =>
  [a] ->
  m (u a)
gchooseFresh [x] = return $ mrgSingle x
gchooseFresh (r : rs) = do
  b <- simpleFresh ()
  res <- gchooseFresh rs
  return $ mrgIf b (mrgSingle r) res
gchooseFresh [] = error "gchooseFresh expects at least one value"

-- | A wrapper for `gchooseFresh` that executes the `MonadFresh` context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
gchoose ::
  forall bool a u.
  ( SymBoolOp bool,
    GMergeable bool a,
    GenSymSimple () bool,
    GMonadUnion bool u
  ) =>
  [a] ->
  FreshIdent ->
  u a
gchoose = runFresh . gchooseFresh

-- | Symbolically chooses one of the provided values.
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it is evaluated, and use
-- these variables to conditionally select one of the @n@ provided expressions.
--
-- The result will __/not/__ be wrapped in a union-like monad, but will be
-- wrapped in a monad maintaining the 'Fresh' context.
--
-- >>> runFresh (gchooseSimpleFresh (Proxy @SymBool) [ssym "b", ssym "c", ssym "d"]) "a" :: SymInteger
-- (ite a@0 b (ite a@1 c d))
gchooseSimpleFresh ::
  forall proxy bool a m.
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GenSymSimple () bool,
    MonadFresh m
  ) =>
  proxy bool ->
  [a] ->
  m a
gchooseSimpleFresh _ [x] = return x
gchooseSimpleFresh proxy (r : rs) = do
  b :: bool <- simpleFresh ()
  res <- gchooseSimpleFresh proxy rs
  return $ gmrgIte b r res
gchooseSimpleFresh _ [] = error "gchooseSimpleFresh expects at least one value"

-- | A wrapper for `gchooseSimpleFresh` that executes the `MonadFresh` context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
gchooseSimple ::
  forall proxy bool a.
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GenSymSimple () bool
  ) =>
  proxy bool ->
  [a] ->
  FreshIdent ->
  a
gchooseSimple p = runFresh . gchooseSimpleFresh p

-- | Symbolically chooses one of the provided values wrapped in union-like
-- monads. The procedure creates @n - 1@ fresh symbolic boolean variables every
-- time it is evaluated, and use these variables to conditionally select one of
-- the @n@ provided expressions.
--
-- The result will be wrapped in a union-like monad, and also a monad
-- maintaining the 'Fresh' context.
--
-- >>> let a = runFresh (gchooseFresh [1, 2]) "a" :: UnionM Integer
-- >>> let b = runFresh (gchooseFresh [2, 3]) "b" :: UnionM Integer
-- >>> runFresh (gchooseUnionFresh [a, b]) "c" :: UnionM Integer
-- UMrg (If (&& c@0 a@0) (Single 1) (If (|| c@0 b@0) (Single 2) (Single 3)))
gchooseUnionFresh ::
  forall bool a m u.
  ( SymBoolOp bool,
    GMergeable bool a,
    GenSymSimple () bool,
    MonadFresh m,
    GMonadUnion bool u
  ) =>
  [u a] ->
  m (u a)
gchooseUnionFresh [x] = return x
gchooseUnionFresh (r : rs) = do
  b <- simpleFresh ()
  res <- gchooseUnionFresh rs
  return $ mrgIf b r res
gchooseUnionFresh [] = error "gchooseUnionFresh expects at least one value"

-- | A wrapper for `gchooseUnionFresh` that executes the `MonadFresh` context.
-- A globally unique identifier should be supplied to ensure the uniqueness of
-- symbolic constants in the generated symbolic values.
gchooseUnion ::
  forall bool a u.
  ( SymBoolOp bool,
    GMergeable bool a,
    GenSymSimple () bool,
    GMonadUnion bool u
  ) =>
  [u a] ->
  FreshIdent ->
  u a
gchooseUnion = runFresh . gchooseUnionFresh

#define CONCRETE_GENSYM_SAME_SHAPE(type) \
instance (SymBoolOp bool, GenSymSimple () bool) => \
  GenSym bool type type where fresh = return . mrgSingle

#define CONCRETE_GENSYMSIMPLE_SAME_SHAPE(type) \
instance GenSymSimple type type where simpleFresh = return

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
CONCRETE_GENSYM_SAME_SHAPE(B.ByteString)

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
CONCRETE_GENSYMSIMPLE_SAME_SHAPE(B.ByteString)
#endif

-- Bool
instance (SymBoolOp bool, GenSymSimple () bool) => GenSym bool () Bool where
  fresh = derivedNoSpecFresh

-- Enums

-- | Specification for enum values with upper bound (exclusive). The result would chosen from [0 .. upperbound].
--
-- >>> runFresh (fresh (EnumGenUpperBound @Integer 4)) "c" :: UnionM Integer
-- UMrg (If c@0 (Single 0) (If c@1 (Single 1) (If c@2 (Single 2) (Single 3))))
newtype EnumGenUpperBound a = EnumGenUpperBound a

instance (SymBoolOp bool, GenSymSimple () bool, Enum v, GMergeable bool v) => GenSym bool (EnumGenUpperBound v) v where
  fresh (EnumGenUpperBound u) = gchooseFresh (toEnum <$> [0 .. fromEnum u - 1])

-- | Specification for numbers with lower bound (inclusive) and upper bound (exclusive)
--
-- >>> runFresh (fresh (EnumGenBound @Integer 0 4)) "c" :: UnionM Integer
-- UMrg (If c@0 (Single 0) (If c@1 (Single 1) (If c@2 (Single 2) (Single 3))))
data EnumGenBound a = EnumGenBound a a

instance (SymBoolOp bool, GenSymSimple () bool, Enum v, GMergeable bool v) => GenSym bool (EnumGenBound v) v where
  fresh (EnumGenBound l u) = gchooseFresh (toEnum <$> [fromEnum l .. fromEnum u - 1])

-- Either
instance
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSymSimple a a,
    GMergeable bool a,
    GenSymSimple b b,
    GMergeable bool b
  ) =>
  GenSym bool (Either a b) (Either a b)

instance
  ( GenSymSimple a a,
    GenSymSimple b b
  ) =>
  GenSymSimple (Either a b) (Either a b)
  where
  simpleFresh = derivedSameShapeSimpleFresh

instance
  (SymBoolOp bool, GenSymSimple () bool, GenSym bool () a, GMergeable bool a, GenSym bool () b, GMergeable bool b) =>
  GenSym bool () (Either a b)
  where
  fresh = derivedNoSpecFresh

-- Maybe
instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple a a, GMergeable bool a) =>
  GenSym bool (Maybe a) (Maybe a)

instance
  (GenSymSimple a a) =>
  GenSymSimple (Maybe a) (Maybe a)
  where
  simpleFresh = derivedSameShapeSimpleFresh

instance (SymBoolOp bool, GenSymSimple () bool, GenSym bool () a, GMergeable bool a) => GenSym bool () (Maybe a) where
  fresh = derivedNoSpecFresh

-- List
instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple () a, GMergeable bool a) =>
  GenSym bool Integer [a]
  where
  fresh v = do
    l <- gl v
    let xs = reverse $ scanr (:) [] l
    gchooseFresh xs
    where
      gl :: (MonadFresh m) => Integer -> m [a]
      gl v1
        | v1 <= 0 = return []
        | otherwise = do
            l <- simpleFresh ()
            r <- gl (v1 - 1)
            return $ l : r

-- | Specification for list generation.
--
-- >>> runFresh (fresh (ListSpec 0 2 ())) "c" :: UnionM [SymBool]
-- UMrg (If c@2 (Single []) (If c@3 (Single [c@1]) (Single [c@0,c@1])))
--
-- >>> runFresh (fresh (ListSpec 0 2 (SimpleListSpec 1 ()))) "c" :: UnionM [[SymBool]]
-- UMrg (If c@2 (Single []) (If c@3 (Single [[c@1]]) (Single [[c@0],[c@1]])))
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
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple spec a, GMergeable bool a) =>
  GenSym bool (ListSpec spec) [a]
  where
  fresh (ListSpec minLen maxLen subSpec) =
    if minLen < 0 || maxLen < 0 || minLen >= maxLen
      then error $ "Bad lengths: " ++ show (minLen, maxLen)
      else do
        l <- gl maxLen
        let xs = drop minLen $ reverse $ scanr (:) [] l
        gchooseFresh xs
    where
      gl :: (MonadFresh m) => Int -> m [a]
      gl currLen
        | currLen <= 0 = return []
        | otherwise = do
            l <- simpleFresh subSpec
            r <- gl (currLen - 1)
            return $ l : r

instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple a a, GMergeable bool a) =>
  GenSym bool [a] [a]

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
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple spec a, GMergeable bool a) =>
  GenSym bool (SimpleListSpec spec) [a]
  where
  fresh = fmap mrgSingle . simpleFresh

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
instance (SymBoolOp bool, GenSymSimple () bool) => GenSym bool () ()

instance GenSymSimple () () where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,)
instance
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool aspec a,
    GMergeable bool a,
    GenSym bool bspec b,
    GMergeable bool b
  ) =>
  GenSym bool (aspec, bspec) (a, b)
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
  (SymBoolOp bool, GenSymSimple () bool, GenSym bool () a, GMergeable bool a, GenSym bool () b, GMergeable bool b) =>
  GenSym bool () (a, b)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool aspec a,
    GMergeable bool a,
    GenSym bool bspec b,
    GMergeable bool b,
    GenSym bool cspec c,
    GMergeable bool c
  ) =>
  GenSym bool (aspec, bspec, cspec) (a, b, c)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool () a,
    GMergeable bool a,
    GenSym bool () b,
    GMergeable bool b,
    GenSym bool () c,
    GMergeable bool c
  ) =>
  GenSym bool () (a, b, c)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool aspec a,
    GMergeable bool a,
    GenSym bool bspec b,
    GMergeable bool b,
    GenSym bool cspec c,
    GMergeable bool c,
    GenSym bool dspec d,
    GMergeable bool d
  ) =>
  GenSym bool (aspec, bspec, cspec, dspec) (a, b, c, d)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool () a,
    GMergeable bool a,
    GenSym bool () b,
    GMergeable bool b,
    GenSym bool () c,
    GMergeable bool c,
    GenSym bool () d,
    GMergeable bool d
  ) =>
  GenSym bool () (a, b, c, d)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool aspec a,
    GMergeable bool a,
    GenSym bool bspec b,
    GMergeable bool b,
    GenSym bool cspec c,
    GMergeable bool c,
    GenSym bool dspec d,
    GMergeable bool d,
    GenSym bool espec e,
    GMergeable bool e
  ) =>
  GenSym bool (aspec, bspec, cspec, dspec, espec) (a, b, c, d, e)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool () a,
    GMergeable bool a,
    GenSym bool () b,
    GMergeable bool b,
    GenSym bool () c,
    GMergeable bool c,
    GenSym bool () d,
    GMergeable bool d,
    GenSym bool () e,
    GMergeable bool e
  ) =>
  GenSym bool () (a, b, c, d, e)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool aspec a,
    GMergeable bool a,
    GenSym bool bspec b,
    GMergeable bool b,
    GenSym bool cspec c,
    GMergeable bool c,
    GenSym bool dspec d,
    GMergeable bool d,
    GenSym bool espec e,
    GMergeable bool e,
    GenSym bool fspec f,
    GMergeable bool f
  ) =>
  GenSym bool (aspec, bspec, cspec, dspec, espec, fspec) (a, b, c, d, e, f)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool () a,
    GMergeable bool a,
    GenSym bool () b,
    GMergeable bool b,
    GenSym bool () c,
    GMergeable bool c,
    GenSym bool () d,
    GMergeable bool d,
    GenSym bool () e,
    GMergeable bool e,
    GenSym bool () f,
    GMergeable bool f
  ) =>
  GenSym bool () (a, b, c, d, e, f)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool aspec a,
    GMergeable bool a,
    GenSym bool bspec b,
    GMergeable bool b,
    GenSym bool cspec c,
    GMergeable bool c,
    GenSym bool dspec d,
    GMergeable bool d,
    GenSym bool espec e,
    GMergeable bool e,
    GenSym bool fspec f,
    GMergeable bool f,
    GenSym bool gspec g,
    GMergeable bool g
  ) =>
  GenSym bool (aspec, bspec, cspec, dspec, espec, fspec, gspec) (a, b, c, d, e, f, g)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool () a,
    GMergeable bool a,
    GenSym bool () b,
    GMergeable bool b,
    GenSym bool () c,
    GMergeable bool c,
    GenSym bool () d,
    GMergeable bool d,
    GenSym bool () e,
    GMergeable bool e,
    GenSym bool () f,
    GMergeable bool f,
    GenSym bool () g,
    GMergeable bool g
  ) =>
  GenSym bool () (a, b, c, d, e, f, g)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool aspec a,
    GMergeable bool a,
    GenSym bool bspec b,
    GMergeable bool b,
    GenSym bool cspec c,
    GMergeable bool c,
    GenSym bool dspec d,
    GMergeable bool d,
    GenSym bool espec e,
    GMergeable bool e,
    GenSym bool fspec f,
    GMergeable bool f,
    GenSym bool gspec g,
    GMergeable bool g,
    GenSym bool hspec h,
    GMergeable bool h
  ) =>
  GenSym bool (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) (a, b, c, d, e, f, g, h)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool () a,
    GMergeable bool a,
    GenSym bool () b,
    GMergeable bool b,
    GenSym bool () c,
    GMergeable bool c,
    GenSym bool () d,
    GMergeable bool d,
    GenSym bool () e,
    GMergeable bool e,
    GenSym bool () f,
    GMergeable bool f,
    GenSym bool () g,
    GMergeable bool g,
    GenSym bool () h,
    GMergeable bool h
  ) =>
  GenSym bool () (a, b, c, d, e, f, g, h)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool spec (m (Maybe a)),
    GMergeable1 bool m,
    GMergeable bool a
  ) =>
  GenSym bool spec (MaybeT m a)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSymSimple (m (Maybe a)) (m (Maybe a)),
    GMergeable1 bool m,
    GMergeable bool a
  ) =>
  GenSym bool (MaybeT m a) (MaybeT m a)

-- ExceptT
instance
  {-# OVERLAPPABLE #-}
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSym bool spec (m (Either a b)),
    GMergeable1 bool m,
    GMergeable bool a,
    GMergeable bool b
  ) =>
  GenSym bool spec (ExceptT a m b)
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
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSymSimple (m (Either e a)) (m (Either e a)),
    GMergeable1 bool m,
    GMergeable bool e,
    GMergeable bool a
  ) =>
  GenSym bool (ExceptT e m a) (ExceptT e m a)
