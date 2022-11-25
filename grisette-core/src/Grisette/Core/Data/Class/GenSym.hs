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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.GenSym
  ( FreshIndex (..),
    FreshIdent,
    pattern FreshIdent,
    pattern FreshIdentWithInfo,
    name,
    nameWithInfo,
    MonadFresh (..),
    FreshT,
    Fresh,
    runFreshT,
    runFresh,
    ggenSym,
    genSymSimple,
    GGenSym (..),
    GenSymSimple (..),
    derivedNoSpecGFresh,
    derivedNoSpecSimpleFresh,
    derivedSameShapeSimpleFresh,
    gchooseFresh,
    gchooseSimpleFresh,
    gchooseUnionFresh,
    gchoose,
    gchooseSimple,
    gchooseUnion,
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
-- Every time a new variable is generated, the index will be increased.
newtype FreshIndex = FreshIndex Int
  deriving (Show)
  deriving (Eq, Ord, Num) via Int

instance (SymBoolOp bool) => GMergeable bool FreshIndex where
  gmergingStrategy = SimpleStrategy $ \_ t f -> max t f

instance (SymBoolOp bool) => GSimpleMergeable bool FreshIndex where
  gmrgIte _ = max

-- | Identifier type used for 'GenSym'
--
-- The constructor is hidden intentionally.
-- You can construct an identifier by:
--
--   * a raw name
--
--     >>> name "a"
--     a
--
--   * bundle the calling file location with the name to ensure global uniqueness
--
-- >>> $$(nameWithLoc "a") -- a sample result could be "a:<interactive>:18:4-18"
-- a:<interactive>:...
--
--   * bundle the calling file location with some user provided information
--
-- >>> nameWithInfo "a" (1 :: Int)
-- a:1
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
-- The user need to ensure uniqueness by themselves if they need to.
name :: String -> FreshIdent
name = FreshIdent

-- | Identifier with extra information.
-- The same name with the same information
-- refers to the same symbolic variable in the whole program.
--
-- The user need to ensure uniqueness by themselves if they need to.
nameWithInfo :: forall a. (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> FreshIdent
nameWithInfo = FreshIdentWithInfo

class Monad m => MonadFresh m where
  nextFreshIndex :: m FreshIndex
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
  gmergingStrategy =
    gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy gmergingStrategy1)) FreshT runFreshT'

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (FreshT m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy2 m gmergingStrategy))))
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
    FreshT $ \ident index -> mergeWithStrategy (liftGMergingStrategy2 s gmergingStrategy) $ f ident index
  mrgIfWithStrategy s cond (FreshT t) (FreshT f) =
    FreshT $ \ident index -> mrgIfWithStrategy (liftGMergingStrategy2 s gmergingStrategy) cond (t ident index) (f ident index)
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

{-
instance (Monad m) => MonadState FreshIndex (FreshT m) where
  state f = FreshT $ \_ idx -> return $ f idx
  put newidx = FreshT $ \_ _ -> return ((), newidx)
  get = FreshT $ \_ idx -> return (idx, idx)

instance (Monad m) => MonadReader FreshIdent (FreshT m) where
  ask = FreshT $ curry return
  local f (FreshT s) = FreshT $ \ident idx -> s (f ident) idx
  reader f = FreshT $ \r s -> return (f r, s)
  -}

-- | Class of types in which symbolic values can be generated with respect to some specification.
--
-- The result will be wrapped in a union-like monad.
-- This ensures that we can generate those types with complex merging rules.
--
-- The uniqueness with be managed with the a monadic context. 'Fresh' and 'FreshT' can be useful.
class (SymBoolOp bool, GMergeable bool a) => GGenSym bool spec a where
  -- | Generate a symbolic value given some specification. The uniqueness is ensured.
  --
  -- The following example generates a symbolic boolean. No specification is needed.
  --
  -- >>> runFresh (gfresh ()) "a" :: UnionM SymBool
  -- UMrg (Single a@0)
  --
  -- The following example generates booleans, which cannot be merged into a single value with type 'Bool'.
  -- No specification is needed.
  --
  -- >>> runFresh (gfresh ()) "a" :: UnionM Bool
  -- UMrg (If a@0 (Single False) (Single True))
  --
  -- The following example generates @Maybe Bool@s.
  -- There are more than one symbolic primitives introduced, and their uniqueness is ensured.
  -- No specification is needed.
  --
  -- >>> runFresh (gfresh ()) "a" :: UnionM (Maybe Bool)
  -- UMrg (If a@0 (Single Nothing) (If a@1 (Single (Just False)) (Single (Just True))))
  --
  -- The following example generates lists of symbolic booleans with length 1 to 2.
  --
  -- >>> runFresh (gfresh (ListSpec 1 2 ())) "a" :: UnionM [SymBool]
  -- UMrg (If a@2 (Single [a@1]) (Single [a@0,a@1]))
  --
  -- When multiple symbolic variables are generated, the uniqueness can be ensured.
  --
  -- >>> runFresh (do; a <- gfresh (); b <- gfresh (); return (a, b)) "a" :: (UnionM SymBool, UnionM SymBool)
  -- (UMrg (Single a@0),UMrg (Single a@1))
  --
  -- N.B.: the examples are not executable solely with @grisette-core@ package, and need support from @grisette-symprim@ package.
  gfresh ::
    ( MonadFresh m,
      GMonadUnion bool u
    ) =>
    spec ->
    m (u a)
  default gfresh ::
    (GenSymSimple spec a) =>
    ( MonadFresh m,
      GMonadUnion bool u
    ) =>
    spec ->
    m (u a)
  gfresh spec = mrgSingle <$> simpleFresh spec

-- | Generate a symbolic variable wrapped in a Union without the monadic context.
-- The uniqueness need to be ensured by the uniqueness of the provided identifier.
ggenSym :: (GGenSym bool spec a, GMonadUnion bool u) => spec -> FreshIdent -> u a
ggenSym = runFresh . gfresh

-- | Class of types in which symbolic values can be generated with respect to some specification.
--
-- The result will __/not/__ be wrapped in a union-like monad.
--
-- The uniqueness with be managed with the a monadic context. 'Fresh' and 'FreshT' can be useful.
class GenSymSimple spec a where
  -- | Generate a symbolic value given some specification. The uniqueness is ensured.
  --
  -- The following example generates a symbolic boolean. No specification is needed.
  --
  -- >>> runFresh (simpleFresh ()) "a" :: SymBool
  -- a@0
  --
  -- The example shows that why the system cannot infer the symbolic boolean type.
  --
  -- >>> runFresh (simpleFresh ()) "a" :: ()
  -- ()
  --
  -- The following code generates list of symbolic boolean with length 2.
  -- As the length is fixed, we don't have to wrap the result in unions.
  --
  -- >>> runFresh (simpleFresh (SimpleListSpec 2 ())) "a" :: [SymBool]
  -- [a@0,a@1]
  --
  -- N.B.: the examples are not executable solely with @grisette-core@ package, and need support from @grisette-symprim@ package.
  simpleFresh ::
    ( MonadFresh m
    ) =>
    spec ->
    m a

-- | Generate a simple symbolic variable wrapped in a Union without the monadic context.
-- The uniqueness need to be ensured by the uniqueness of the provided identifier.
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

instance (SymBoolOp bool, GGenSym bool () c) => GenSymNoSpec bool (K1 i c) where
  freshNoSpec = fmap K1 <$> gfresh ()

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

-- | We cannot provide DerivingVia style derivation for 'GenSym'.
--
-- This 'fresh' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with '()' as specification,
-- and generate all possible values for a sum type.
--
-- N.B. Never use on recursive types
derivedNoSpecGFresh ::
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
derivedNoSpecGFresh _ = merge . fmap to <$> freshNoSpec

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

-- | We cannot provide DerivingVia style derivation for 'GenSymSimple'.
--
-- This 'simpleFresh' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with '()' as specification.
-- It will not work on sum types.
--
-- N.B. Never use on recursive types
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

-- | We cannot provide DerivingVia style derivation for 'GenSymSimple'.
--
-- This 'simpleFresh' implementation is for the types that can be generated with a reference value of the same type.
--
-- For sum types, it will generate the result with the same data constructor.
-- For product types, it will generate the result by generating each field with the corresponding reference value.
--
-- N.B. Can be used on recursive types
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
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it is evaluated, and use
-- these variables to conditionally select one of the @n@ provided expressions.
--
-- The result will be wrapped in a union-like monad, and also a monad maintaining the 'GenSym' context.
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
-- The result will __/not/__ be wrapped in a union-like monad, but will be wrapped in a monad maintaining the 'GenSym' context.
-- Similar to 'simpleFresh', you need to tell the system what symbolic boolean type to use.
--
-- >>> runFresh (gchooseSimpleFresh (Proxy @SymBool) [ssymb "b", ssymb "c", ssymb "d"]) "a" :: SymInteger
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

-- | Symbolically chooses one of the provided values wrapped in union-like monads.
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it is evaluated, and use
-- these variables to conditionally select one of the @n@ provided expressions.
--
-- The result will be wrapped in a union-like monad, and also a monad maintaining the 'GenSym' context.
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

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Bool Bool where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Integer Integer where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Char Char where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int Int where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int8 Int8 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int16 Int16 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int32 Int32 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int64 Int64 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word Word where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word8 Word8 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word16 Word16 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word32 Word32 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word64 Word64 where gfresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool B.ByteString B.ByteString where gfresh = return . mrgSingle

instance GenSymSimple Bool Bool where simpleFresh = return

instance GenSymSimple Integer Integer where simpleFresh = return

instance GenSymSimple Char Char where simpleFresh = return

instance GenSymSimple Int Int where simpleFresh = return

instance GenSymSimple Int8 Int8 where simpleFresh = return

instance GenSymSimple Int16 Int16 where simpleFresh = return

instance GenSymSimple Int32 Int32 where simpleFresh = return

instance GenSymSimple Int64 Int64 where simpleFresh = return

instance GenSymSimple Word Word where simpleFresh = return

instance GenSymSimple Word8 Word8 where simpleFresh = return

instance GenSymSimple Word16 Word16 where simpleFresh = return

instance GenSymSimple Word32 Word32 where simpleFresh = return

instance GenSymSimple Word64 Word64 where simpleFresh = return

instance GenSymSimple B.ByteString B.ByteString where simpleFresh = return

-- Bool
instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool () Bool where
  gfresh = derivedNoSpecGFresh

-- Enums

-- | Specification for enum values with upper bound (exclusive). The result would chosen from [0 .. upperbound].
--
-- >>> runFresh (gfresh (EnumGenUpperBound @Integer 4)) "c" :: UnionM Integer
-- UMrg (If c@0 (Single 0) (If c@1 (Single 1) (If c@2 (Single 2) (Single 3))))
newtype EnumGenUpperBound a = EnumGenUpperBound a

instance (SymBoolOp bool, GenSymSimple () bool, Enum v, GMergeable bool v) => GGenSym bool (EnumGenUpperBound v) v where
  gfresh (EnumGenUpperBound u) = gchooseFresh (toEnum <$> [0 .. fromEnum u - 1])

-- | Specification for numbers with lower bound (inclusive) and upper bound (exclusive)
--
-- >>> runFresh (gfresh (EnumGenBound @Integer 0 4)) "c" :: UnionM Integer
-- UMrg (If c@0 (Single 0) (If c@1 (Single 1) (If c@2 (Single 2) (Single 3))))
data EnumGenBound a = EnumGenBound a a

instance (SymBoolOp bool, GenSymSimple () bool, Enum v, GMergeable bool v) => GGenSym bool (EnumGenBound v) v where
  gfresh (EnumGenBound l u) = gchooseFresh (toEnum <$> [fromEnum l .. fromEnum u - 1])

-- Either
instance
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GenSymSimple a a,
    GMergeable bool a,
    GenSymSimple b b,
    GMergeable bool b
  ) =>
  GGenSym bool (Either a b) (Either a b)

instance
  ( GenSymSimple a a,
    GenSymSimple b b
  ) =>
  GenSymSimple (Either a b) (Either a b)
  where
  simpleFresh = derivedSameShapeSimpleFresh

instance
  (SymBoolOp bool, GenSymSimple () bool, GGenSym bool () a, GMergeable bool a, GGenSym bool () b, GMergeable bool b) =>
  GGenSym bool () (Either a b)
  where
  gfresh = derivedNoSpecGFresh

-- Maybe
instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple a a, GMergeable bool a) =>
  GGenSym bool (Maybe a) (Maybe a)

instance
  (GenSymSimple a a) =>
  GenSymSimple (Maybe a) (Maybe a)
  where
  simpleFresh = derivedSameShapeSimpleFresh

instance (SymBoolOp bool, GenSymSimple () bool, GGenSym bool () a, GMergeable bool a) => GGenSym bool () (Maybe a) where
  gfresh = derivedNoSpecGFresh

-- List
instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple () a, GMergeable bool a) =>
  GGenSym bool Integer [a]
  where
  gfresh v = do
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
-- >>> runFresh (gfresh (ListSpec 0 2 ())) "c" :: UnionM [SymBool]
-- UMrg (If c@2 (Single []) (If c@3 (Single [c@1]) (Single [c@0,c@1])))
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
  GGenSym bool (ListSpec spec) [a]
  where
  gfresh (ListSpec minLen maxLen subSpec) =
    if minLen < 0 || maxLen < 0 || minLen >= maxLen
      then error $ "Bad lengthes: " ++ show (minLen, maxLen)
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
  GGenSym bool [a] [a]

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
  GGenSym bool (SimpleListSpec spec) [a]
  where
  gfresh = fmap mrgSingle . simpleFresh

instance
  (GenSymSimple spec a) =>
  GenSymSimple (SimpleListSpec spec) [a]
  where
  simpleFresh (SimpleListSpec len subSpec) =
    if len < 0
      then error $ "Bad lengthes: " ++ show len
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
instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool () ()

instance GenSymSimple () () where
  simpleFresh = derivedNoSpecSimpleFresh

-- (,)
instance
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GGenSym bool aspec a,
    GMergeable bool a,
    GGenSym bool bspec b,
    GMergeable bool b
  ) =>
  GGenSym bool (aspec, bspec) (a, b)
  where
  gfresh (aspec, bspec) = do
    a1 <- gfresh aspec
    b1 <- gfresh bspec
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
  (SymBoolOp bool, GenSymSimple () bool, GGenSym bool () a, GMergeable bool a, GGenSym bool () b, GMergeable bool b) =>
  GGenSym bool () (a, b)
  where
  gfresh = derivedNoSpecGFresh

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
    GGenSym bool aspec a,
    GMergeable bool a,
    GGenSym bool bspec b,
    GMergeable bool b,
    GGenSym bool cspec c,
    GMergeable bool c
  ) =>
  GGenSym bool (aspec, bspec, cspec) (a, b, c)
  where
  gfresh (aspec, bspec, cspec) = do
    a1 <- gfresh aspec
    b1 <- gfresh bspec
    c1 <- gfresh cspec
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
    GGenSym bool () a,
    GMergeable bool a,
    GGenSym bool () b,
    GMergeable bool b,
    GGenSym bool () c,
    GMergeable bool c
  ) =>
  GGenSym bool () (a, b, c)
  where
  gfresh = derivedNoSpecGFresh

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
    GGenSym bool aspec a,
    GMergeable bool a,
    GGenSym bool bspec b,
    GMergeable bool b,
    GGenSym bool cspec c,
    GMergeable bool c,
    GGenSym bool dspec d,
    GMergeable bool d
  ) =>
  GGenSym bool (aspec, bspec, cspec, dspec) (a, b, c, d)
  where
  gfresh (aspec, bspec, cspec, dspec) = do
    a1 <- gfresh aspec
    b1 <- gfresh bspec
    c1 <- gfresh cspec
    d1 <- gfresh dspec
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
    GGenSym bool () a,
    GMergeable bool a,
    GGenSym bool () b,
    GMergeable bool b,
    GGenSym bool () c,
    GMergeable bool c,
    GGenSym bool () d,
    GMergeable bool d
  ) =>
  GGenSym bool () (a, b, c, d)
  where
  gfresh = derivedNoSpecGFresh

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
    GGenSym bool aspec a,
    GMergeable bool a,
    GGenSym bool bspec b,
    GMergeable bool b,
    GGenSym bool cspec c,
    GMergeable bool c,
    GGenSym bool dspec d,
    GMergeable bool d,
    GGenSym bool espec e,
    GMergeable bool e
  ) =>
  GGenSym bool (aspec, bspec, cspec, dspec, espec) (a, b, c, d, e)
  where
  gfresh (aspec, bspec, cspec, dspec, espec) = do
    a1 <- gfresh aspec
    b1 <- gfresh bspec
    c1 <- gfresh cspec
    d1 <- gfresh dspec
    e1 <- gfresh espec
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
    GGenSym bool () a,
    GMergeable bool a,
    GGenSym bool () b,
    GMergeable bool b,
    GGenSym bool () c,
    GMergeable bool c,
    GGenSym bool () d,
    GMergeable bool d,
    GGenSym bool () e,
    GMergeable bool e
  ) =>
  GGenSym bool () (a, b, c, d, e)
  where
  gfresh = derivedNoSpecGFresh

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
    GGenSym bool aspec a,
    GMergeable bool a,
    GGenSym bool bspec b,
    GMergeable bool b,
    GGenSym bool cspec c,
    GMergeable bool c,
    GGenSym bool dspec d,
    GMergeable bool d,
    GGenSym bool espec e,
    GMergeable bool e,
    GGenSym bool fspec f,
    GMergeable bool f
  ) =>
  GGenSym bool (aspec, bspec, cspec, dspec, espec, fspec) (a, b, c, d, e, f)
  where
  gfresh (aspec, bspec, cspec, dspec, espec, fspec) = do
    a1 <- gfresh aspec
    b1 <- gfresh bspec
    c1 <- gfresh cspec
    d1 <- gfresh dspec
    e1 <- gfresh espec
    f1 <- gfresh fspec
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
    GGenSym bool () a,
    GMergeable bool a,
    GGenSym bool () b,
    GMergeable bool b,
    GGenSym bool () c,
    GMergeable bool c,
    GGenSym bool () d,
    GMergeable bool d,
    GGenSym bool () e,
    GMergeable bool e,
    GGenSym bool () f,
    GMergeable bool f
  ) =>
  GGenSym bool () (a, b, c, d, e, f)
  where
  gfresh = derivedNoSpecGFresh

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
    GGenSym bool aspec a,
    GMergeable bool a,
    GGenSym bool bspec b,
    GMergeable bool b,
    GGenSym bool cspec c,
    GMergeable bool c,
    GGenSym bool dspec d,
    GMergeable bool d,
    GGenSym bool espec e,
    GMergeable bool e,
    GGenSym bool fspec f,
    GMergeable bool f,
    GGenSym bool gspec g,
    GMergeable bool g
  ) =>
  GGenSym bool (aspec, bspec, cspec, dspec, espec, fspec, gspec) (a, b, c, d, e, f, g)
  where
  gfresh (aspec, bspec, cspec, dspec, espec, fspec, gspec) = do
    a1 <- gfresh aspec
    b1 <- gfresh bspec
    c1 <- gfresh cspec
    d1 <- gfresh dspec
    e1 <- gfresh espec
    f1 <- gfresh fspec
    g1 <- gfresh gspec
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
    GGenSym bool () a,
    GMergeable bool a,
    GGenSym bool () b,
    GMergeable bool b,
    GGenSym bool () c,
    GMergeable bool c,
    GGenSym bool () d,
    GMergeable bool d,
    GGenSym bool () e,
    GMergeable bool e,
    GGenSym bool () f,
    GMergeable bool f,
    GGenSym bool () g,
    GMergeable bool g
  ) =>
  GGenSym bool () (a, b, c, d, e, f, g)
  where
  gfresh = derivedNoSpecGFresh

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
    GGenSym bool aspec a,
    GMergeable bool a,
    GGenSym bool bspec b,
    GMergeable bool b,
    GGenSym bool cspec c,
    GMergeable bool c,
    GGenSym bool dspec d,
    GMergeable bool d,
    GGenSym bool espec e,
    GMergeable bool e,
    GGenSym bool fspec f,
    GMergeable bool f,
    GGenSym bool gspec g,
    GMergeable bool g,
    GGenSym bool hspec h,
    GMergeable bool h
  ) =>
  GGenSym bool (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) (a, b, c, d, e, f, g, h)
  where
  gfresh (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) = do
    a1 <- gfresh aspec
    b1 <- gfresh bspec
    c1 <- gfresh cspec
    d1 <- gfresh dspec
    e1 <- gfresh espec
    f1 <- gfresh fspec
    g1 <- gfresh gspec
    h1 <- gfresh hspec
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
    GGenSym bool () a,
    GMergeable bool a,
    GGenSym bool () b,
    GMergeable bool b,
    GGenSym bool () c,
    GMergeable bool c,
    GGenSym bool () d,
    GMergeable bool d,
    GGenSym bool () e,
    GMergeable bool e,
    GGenSym bool () f,
    GMergeable bool f,
    GGenSym bool () g,
    GMergeable bool g,
    GGenSym bool () h,
    GMergeable bool h
  ) =>
  GGenSym bool () (a, b, c, d, e, f, g, h)
  where
  gfresh = derivedNoSpecGFresh

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
    GGenSym bool spec (m (Maybe a)),
    GMergeable1 bool m,
    GMergeable bool a
  ) =>
  GGenSym bool spec (MaybeT m a)
  where
  gfresh v = do
    x <- gfresh v
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
  GGenSym bool (MaybeT m a) (MaybeT m a)

-- ExceptT
instance
  {-# OVERLAPPABLE #-}
  ( SymBoolOp bool,
    GenSymSimple () bool,
    GGenSym bool spec (m (Either a b)),
    GMergeable1 bool m,
    GMergeable bool a,
    GMergeable bool b
  ) =>
  GGenSym bool spec (ExceptT a m b)
  where
  gfresh v = do
    x <- gfresh v
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
  GGenSym bool (ExceptT e m a) (ExceptT e m a)
