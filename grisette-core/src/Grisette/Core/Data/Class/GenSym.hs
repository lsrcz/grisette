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
  ( GenSymIndex (..),
    GenSymIdent,
    pattern GenSymIdent,
    pattern GenSymIdentWithInfo,
    name,
    nameWithInfo,
    MonadGenSymFresh (..),
    GenSymFreshT,
    GenSymFresh,
    runGenSymFreshT,
    runGenSymFresh,
    ggenSym,
    genSymSimple,
    GGenSym (..),
    GenSymSimple (..),
    derivedNoSpecGGenSymFresh,
    derivedNoSpecGenSymSimpleFresh,
    derivedSameShapeGenSymSimpleFresh,
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
newtype GenSymIndex = GenSymIndex Int
  deriving (Show)
  deriving (Eq, Ord, Num) via Int

instance (SymBoolOp bool) => GMergeable bool GenSymIndex where
  gmergingStrategy = SimpleStrategy $ \_ t f -> max t f

instance (SymBoolOp bool) => GSimpleMergeable bool GenSymIndex where
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
data GenSymIdent where
  GenSymIdent :: String -> GenSymIdent
  GenSymIdentWithInfo :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> GenSymIdent

instance Show GenSymIdent where
  show (GenSymIdent i) = i
  show (GenSymIdentWithInfo s i) = s ++ ":" ++ show i

instance IsString GenSymIdent where
  fromString = name

instance Eq GenSymIdent where
  GenSymIdent l == GenSymIdent r = l == r
  GenSymIdentWithInfo l (linfo :: linfo) == GenSymIdentWithInfo r (rinfo :: rinfo) = case eqT @linfo @rinfo of
    Just Refl -> l == r && linfo == rinfo
    _ -> False
  _ == _ = False

instance Ord GenSymIdent where
  GenSymIdent l <= GenSymIdent r = l <= r
  GenSymIdent _ <= _ = True
  _ <= GenSymIdent _ = False
  GenSymIdentWithInfo l (linfo :: linfo) <= GenSymIdentWithInfo r (rinfo :: rinfo) =
    l < r
      || ( l == r
             && ( case eqT @linfo @rinfo of
                    Just Refl -> linfo <= rinfo
                    _ -> typeRep (Proxy @linfo) <= typeRep (Proxy @rinfo)
                )
         )

instance Hashable GenSymIdent where
  hashWithSalt s (GenSymIdent n) = s `hashWithSalt` n
  hashWithSalt s (GenSymIdentWithInfo n i) = s `hashWithSalt` n `hashWithSalt` i

instance Lift GenSymIdent where
  liftTyped (GenSymIdent n) = [||GenSymIdent n||]
  liftTyped (GenSymIdentWithInfo n i) = [||GenSymIdentWithInfo n i||]

instance NFData GenSymIdent where
  rnf (GenSymIdent n) = rnf n
  rnf (GenSymIdentWithInfo n i) = rnf n `seq` rnf i

-- | Simple name identifier.
-- The same identifier refers to the same symbolic variable in the whole program.
--
-- The user need to ensure uniqueness by themselves if they need to.
name :: String -> GenSymIdent
name = GenSymIdent

-- | Identifier with extra information.
-- The same name with the same information
-- refers to the same symbolic variable in the whole program.
--
-- The user need to ensure uniqueness by themselves if they need to.
nameWithInfo :: forall a. (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> GenSymIdent
nameWithInfo = GenSymIdentWithInfo

class Monad m => MonadGenSymFresh m where
  nextGenSymIndex :: m GenSymIndex
  getGenSymIdent :: m GenSymIdent

-- | A symbolic generation monad transformer.
-- It is a reader monad transformer for identifiers and
-- a state monad transformer for indices.
--
-- Each time a fresh symbolic variable is generated, the index should be increased.
newtype GenSymFreshT m a = GenSymFreshT {runGenSymFreshT' :: GenSymIdent -> GenSymIndex -> m (a, GenSymIndex)}

instance
  (SymBoolOp bool, GMergeable bool a, GMergeable1 bool m) =>
  GMergeable bool (GenSymFreshT m a)
  where
  gmergingStrategy =
    gwrapStrategy (liftGMergingStrategy (liftGMergingStrategy gmergingStrategy1)) GenSymFreshT runGenSymFreshT'

instance (SymBoolOp bool, GMergeable1 bool m) => GMergeable1 bool (GenSymFreshT m) where
  liftGMergingStrategy m =
    gwrapStrategy
      (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy (liftGMergingStrategy2 m gmergingStrategy))))
      GenSymFreshT
      runGenSymFreshT'

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a) =>
  GSimpleMergeable bool (GenSymFreshT m a)
  where
  gmrgIte = mrgIf

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable1 bool (GenSymFreshT m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GUnionLike bool (GenSymFreshT m)
  where
  mergeWithStrategy s (GenSymFreshT f) =
    GenSymFreshT $ \ident index -> mergeWithStrategy (liftGMergingStrategy2 s gmergingStrategy) $ f ident index
  mrgIfWithStrategy s cond (GenSymFreshT t) (GenSymFreshT f) =
    GenSymFreshT $ \ident index -> mrgIfWithStrategy (liftGMergingStrategy2 s gmergingStrategy) cond (t ident index) (f ident index)
  single x = GenSymFreshT $ \_ i -> single (x, i)
  unionIf cond (GenSymFreshT t) (GenSymFreshT f) =
    GenSymFreshT $ \ident index -> unionIf cond (t ident index) (f ident index)

-- | Run the symbolic generation with the given identifier and 0 as the initial index.
runGenSymFreshT :: (Monad m) => GenSymFreshT m a -> GenSymIdent -> m a
runGenSymFreshT m ident = fst <$> runGenSymFreshT' m ident (GenSymIndex 0)

instance (Functor f) => Functor (GenSymFreshT f) where
  fmap f (GenSymFreshT s) = GenSymFreshT $ \ident idx -> first f <$> s ident idx

instance (Applicative m, Monad m) => Applicative (GenSymFreshT m) where
  pure a = GenSymFreshT $ \_ idx -> pure (a, idx)
  GenSymFreshT fs <*> GenSymFreshT as = GenSymFreshT $ \ident idx -> do
    (f, idx') <- fs ident idx
    (a, idx'') <- as ident idx'
    return (f a, idx'')

instance (Monad m) => Monad (GenSymFreshT m) where
  (GenSymFreshT s) >>= f = GenSymFreshT $ \ident idx -> do
    (a, idx') <- s ident idx
    runGenSymFreshT' (f a) ident idx'

instance MonadTrans GenSymFreshT where
  lift x = GenSymFreshT $ \_ index -> (,index) <$> x

liftGenSymFreshTCache :: (Functor m) => Catch e m (a, GenSymIndex) -> Catch e (GenSymFreshT m) a
liftGenSymFreshTCache catchE (GenSymFreshT m) h =
  GenSymFreshT $ \ident index -> m ident index `catchE` \e -> runGenSymFreshT' (h e) ident index

instance (MonadError e m) => MonadError e (GenSymFreshT m) where
  throwError = lift . throwError
  catchError = liftGenSymFreshTCache catchError

-- | 'GenSymFreshT' specialized with Identity.
type GenSymFresh = GenSymFreshT Identity

-- | Run the symbolic generation with the given identifier and 0 as the initial index.
runGenSymFresh :: GenSymFresh a -> GenSymIdent -> a
runGenSymFresh m ident = runIdentity $ runGenSymFreshT m ident

instance Monad m => MonadGenSymFresh (GenSymFreshT m) where
  nextGenSymIndex = GenSymFreshT $ \_ idx -> return (idx, idx + 1)
  getGenSymIdent = GenSymFreshT $ curry return

{-
instance (Monad m) => MonadState GenSymIndex (GenSymFreshT m) where
  state f = GenSymFreshT $ \_ idx -> return $ f idx
  put newidx = GenSymFreshT $ \_ _ -> return ((), newidx)
  get = GenSymFreshT $ \_ idx -> return (idx, idx)

instance (Monad m) => MonadReader GenSymIdent (GenSymFreshT m) where
  ask = GenSymFreshT $ curry return
  local f (GenSymFreshT s) = GenSymFreshT $ \ident idx -> s (f ident) idx
  reader f = GenSymFreshT $ \r s -> return (f r, s)
  -}

-- | Class of types in which symbolic values can be generated with respect to some specification.
--
-- The result will be wrapped in a union-like monad.
-- This ensures that we can generate those types with complex merging rules.
--
-- The uniqueness with be managed with the a monadic context. 'GenSymFresh' and 'GenSymFreshT' can be useful.
class (SymBoolOp bool, GMergeable bool a) => GGenSym bool spec a where
  -- | Generate a symbolic value given some specification. The uniqueness is ensured.
  --
  -- The following example generates a symbolic boolean. No specification is needed.
  --
  -- >>> runGenSymFresh (ggenSymFresh ()) "a" :: UnionM SymBool
  -- UMrg (Single a@0)
  --
  -- The following example generates booleans, which cannot be merged into a single value with type 'Bool'.
  -- No specification is needed.
  --
  -- >>> runGenSymFresh (ggenSymFresh ()) "a" :: UnionM Bool
  -- UMrg (If a@0 (Single False) (Single True))
  --
  -- The following example generates @Maybe Bool@s.
  -- There are more than one symbolic primitives introduced, and their uniqueness is ensured.
  -- No specification is needed.
  --
  -- >>> runGenSymFresh (ggenSymFresh ()) "a" :: UnionM (Maybe Bool)
  -- UMrg (If a@0 (Single Nothing) (If a@1 (Single (Just False)) (Single (Just True))))
  --
  -- The following example generates lists of symbolic booleans with length 1 to 2.
  --
  -- >>> runGenSymFresh (ggenSymFresh (ListSpec 1 2 ())) "a" :: UnionM [SymBool]
  -- UMrg (If a@2 (Single [a@1]) (Single [a@0,a@1]))
  --
  -- When multiple symbolic variables are generated, the uniqueness can be ensured.
  --
  -- >>> runGenSymFresh (do; a <- ggenSymFresh (); b <- ggenSymFresh (); return (a, b)) "a" :: (UnionM SymBool, UnionM SymBool)
  -- (UMrg (Single a@0),UMrg (Single a@1))
  --
  -- N.B.: the examples are not executable solely with @grisette-core@ package, and need support from @grisette-symprim@ package.
  ggenSymFresh ::
    ( MonadGenSymFresh m,
      GMonadUnion bool u
    ) =>
    spec ->
    m (u a)
  default ggenSymFresh ::
    (GenSymSimple spec a) =>
    ( MonadGenSymFresh m,
      GMonadUnion bool u
    ) =>
    spec ->
    m (u a)
  ggenSymFresh spec = mrgSingle <$> genSymSimpleFresh spec

-- | Generate a symbolic variable wrapped in a Union without the monadic context.
-- The uniqueness need to be ensured by the uniqueness of the provided identifier.
ggenSym :: (GGenSym bool spec a, GMonadUnion bool u) => spec -> GenSymIdent -> u a
ggenSym = runGenSymFresh . ggenSymFresh

-- | Class of types in which symbolic values can be generated with respect to some specification.
--
-- The result will __/not/__ be wrapped in a union-like monad.
--
-- The uniqueness with be managed with the a monadic context. 'GenSymFresh' and 'GenSymFreshT' can be useful.
class GenSymSimple spec a where
  -- | Generate a symbolic value given some specification. The uniqueness is ensured.
  --
  -- The following example generates a symbolic boolean. No specification is needed.
  --
  -- >>> runGenSymFresh (genSymSimpleFresh ()) "a" :: SymBool
  -- a@0
  --
  -- The example shows that why the system cannot infer the symbolic boolean type.
  --
  -- >>> runGenSymFresh (genSymSimpleFresh ()) "a" :: ()
  -- ()
  --
  -- The following code generates list of symbolic boolean with length 2.
  -- As the length is fixed, we don't have to wrap the result in unions.
  --
  -- >>> runGenSymFresh (genSymSimpleFresh (SimpleListSpec 2 ())) "a" :: [SymBool]
  -- [a@0,a@1]
  --
  -- N.B.: the examples are not executable solely with @grisette-core@ package, and need support from @grisette-symprim@ package.
  genSymSimpleFresh ::
    ( MonadGenSymFresh m
    ) =>
    spec ->
    m a

-- | Generate a simple symbolic variable wrapped in a Union without the monadic context.
-- The uniqueness need to be ensured by the uniqueness of the provided identifier.
genSymSimple :: forall spec a. (GenSymSimple spec a) => spec -> GenSymIdent -> a
genSymSimple = runGenSymFresh . genSymSimpleFresh

class GenSymNoSpec bool a where
  genSymFreshNoSpec ::
    ( MonadGenSymFresh m,
      GMonadUnion bool u
    ) =>
    m (u (a c))

instance (SymBoolOp bool) => GenSymNoSpec bool U1 where
  genSymFreshNoSpec = return $ mrgSingle U1

instance (SymBoolOp bool, GGenSym bool () c) => GenSymNoSpec bool (K1 i c) where
  genSymFreshNoSpec = fmap K1 <$> ggenSymFresh ()

instance (SymBoolOp bool, GenSymNoSpec bool a) => GenSymNoSpec bool (M1 i c a) where
  genSymFreshNoSpec = fmap M1 <$> genSymFreshNoSpec

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
  genSymFreshNoSpec ::
    forall m u c.
    ( MonadGenSymFresh m,
      GMonadUnion bool u
    ) =>
    m (u ((a :+: b) c))
  genSymFreshNoSpec = do
    cond :: bool <- genSymSimpleFresh ()
    l :: u (a c) <- genSymFreshNoSpec
    r :: u (b c) <- genSymFreshNoSpec
    return $ mrgIf cond (fmap L1 l) (fmap R1 r)

instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymNoSpec bool a, GenSymNoSpec bool b) =>
  GenSymNoSpec bool (a :*: b)
  where
  genSymFreshNoSpec ::
    forall m u c.
    ( MonadGenSymFresh m,
      GMonadUnion bool u
    ) =>
    m (u ((a :*: b) c))
  genSymFreshNoSpec = do
    l :: u (a c) <- genSymFreshNoSpec
    r :: u (b c) <- genSymFreshNoSpec
    return $ do
      l1 <- l
      r1 <- r
      return $ l1 :*: r1

-- | We cannot provide DerivingVia style derivation for 'GenSym'.
--
-- This 'genSymFresh' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with '()' as specification,
-- and generate all possible values for a sum type.
--
-- N.B. Never use on recursive types
derivedNoSpecGGenSymFresh ::
  forall bool a m u.
  ( Generic a,
    SymBoolOp bool,
    GenSymNoSpec bool (Rep a),
    GMergeable bool a,
    MonadGenSymFresh m,
    GMonadUnion bool u
  ) =>
  () ->
  m (u a)
derivedNoSpecGGenSymFresh _ = merge . fmap to <$> genSymFreshNoSpec

class GenSymSimpleNoSpec a where
  genSymSimpleFreshNoSpec :: (MonadGenSymFresh m) => m (a c)

instance GenSymSimpleNoSpec U1 where
  genSymSimpleFreshNoSpec = return U1

instance (GenSymSimple () c) => GenSymSimpleNoSpec (K1 i c) where
  genSymSimpleFreshNoSpec = K1 <$> genSymSimpleFresh ()

instance (GenSymSimpleNoSpec a) => GenSymSimpleNoSpec (M1 i c a) where
  genSymSimpleFreshNoSpec = M1 <$> genSymSimpleFreshNoSpec

instance
  (GenSymSimpleNoSpec a, GenSymSimpleNoSpec b) =>
  GenSymSimpleNoSpec (a :*: b)
  where
  genSymSimpleFreshNoSpec = do
    l :: a c <- genSymSimpleFreshNoSpec
    r :: b c <- genSymSimpleFreshNoSpec
    return $ l :*: r

-- | We cannot provide DerivingVia style derivation for 'GenSymSimple'.
--
-- This 'genSymSimpleFresh' implementation is for the types that does not need any specification.
-- It will generate product types by generating each fields with '()' as specification.
-- It will not work on sum types.
--
-- N.B. Never use on recursive types
derivedNoSpecGenSymSimpleFresh ::
  forall a m.
  ( Generic a,
    GenSymSimpleNoSpec (Rep a),
    MonadGenSymFresh m
  ) =>
  () ->
  m a
derivedNoSpecGenSymSimpleFresh _ = to <$> genSymSimpleFreshNoSpec

class GenSymSameShape a where
  genSymSameShapeFresh ::
    ( MonadGenSymFresh m
    ) =>
    a c ->
    m (a c)

instance GenSymSameShape U1 where
  genSymSameShapeFresh _ = return U1

instance (GenSymSimple c c) => GenSymSameShape (K1 i c) where
  genSymSameShapeFresh (K1 c) = K1 <$> genSymSimpleFresh c

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
-- This 'genSymSimpleFresh' implementation is for the types that can be generated with a reference value of the same type.
--
-- For sum types, it will generate the result with the same data constructor.
-- For product types, it will generate the result by generating each field with the corresponding reference value.
--
-- N.B. Can be used on recursive types
derivedSameShapeGenSymSimpleFresh ::
  forall a m.
  ( Generic a,
    GenSymSameShape (Rep a),
    MonadGenSymFresh m
  ) =>
  a ->
  m a
derivedSameShapeGenSymSimpleFresh a = to <$> genSymSameShapeFresh (from a)

-- | Symbolically chooses one of the provided values.
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it is evaluated, and use
-- these variables to conditionally select one of the @n@ provided expressions.
--
-- The result will be wrapped in a union-like monad, and also a monad maintaining the 'GenSym' context.
--
-- >>> runGenSymFresh (gchooseFresh [1,2,3]) "a" :: UnionM Integer
-- UMrg (If a@0 (Single 1) (If a@1 (Single 2) (Single 3)))
gchooseFresh ::
  forall bool a m u.
  ( SymBoolOp bool,
    GMergeable bool a,
    GenSymSimple () bool,
    MonadGenSymFresh m,
    GMonadUnion bool u
  ) =>
  [a] ->
  m (u a)
gchooseFresh [x] = return $ mrgSingle x
gchooseFresh (r : rs) = do
  b <- genSymSimpleFresh ()
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
  GenSymIdent ->
  u a
gchoose = runGenSymFresh . gchooseFresh

-- | Symbolically chooses one of the provided values.
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it is evaluated, and use
-- these variables to conditionally select one of the @n@ provided expressions.
--
-- The result will __/not/__ be wrapped in a union-like monad, but will be wrapped in a monad maintaining the 'GenSym' context.
-- Similar to 'genSymSimpleFresh', you need to tell the system what symbolic boolean type to use.
--
-- >>> runGenSymFresh (gchooseSimpleFresh (Proxy @SymBool) [ssymb "b", ssymb "c", ssymb "d"]) "a" :: SymInteger
-- (ite a@0 b (ite a@1 c d))
gchooseSimpleFresh ::
  forall proxy bool a m.
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GenSymSimple () bool,
    MonadGenSymFresh m
  ) =>
  proxy bool ->
  [a] ->
  m a
gchooseSimpleFresh _ [x] = return x
gchooseSimpleFresh proxy (r : rs) = do
  b :: bool <- genSymSimpleFresh ()
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
  GenSymIdent ->
  a
gchooseSimple p = runGenSymFresh . gchooseSimpleFresh p

-- | Symbolically chooses one of the provided values wrapped in union-like monads.
-- The procedure creates @n - 1@ fresh symbolic boolean variables every time it is evaluated, and use
-- these variables to conditionally select one of the @n@ provided expressions.
--
-- The result will be wrapped in a union-like monad, and also a monad maintaining the 'GenSym' context.
--
-- >>> let a = runGenSymFresh (gchooseFresh [1, 2]) "a" :: UnionM Integer
-- >>> let b = runGenSymFresh (gchooseFresh [2, 3]) "b" :: UnionM Integer
-- >>> runGenSymFresh (gchooseUnionFresh [a, b]) "c" :: UnionM Integer
-- UMrg (If (&& c@0 a@0) (Single 1) (If (|| c@0 b@0) (Single 2) (Single 3)))
gchooseUnionFresh ::
  forall bool a m u.
  ( SymBoolOp bool,
    GMergeable bool a,
    GenSymSimple () bool,
    MonadGenSymFresh m,
    GMonadUnion bool u
  ) =>
  [u a] ->
  m (u a)
gchooseUnionFresh [x] = return x
gchooseUnionFresh (r : rs) = do
  b <- genSymSimpleFresh ()
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
  GenSymIdent ->
  u a
gchooseUnion = runGenSymFresh . gchooseUnionFresh

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Bool Bool where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Integer Integer where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Char Char where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int Int where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int8 Int8 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int16 Int16 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int32 Int32 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Int64 Int64 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word Word where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word8 Word8 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word16 Word16 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word32 Word32 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool Word64 Word64 where ggenSymFresh = return . mrgSingle

instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool B.ByteString B.ByteString where ggenSymFresh = return . mrgSingle

instance GenSymSimple Bool Bool where genSymSimpleFresh = return

instance GenSymSimple Integer Integer where genSymSimpleFresh = return

instance GenSymSimple Char Char where genSymSimpleFresh = return

instance GenSymSimple Int Int where genSymSimpleFresh = return

instance GenSymSimple Int8 Int8 where genSymSimpleFresh = return

instance GenSymSimple Int16 Int16 where genSymSimpleFresh = return

instance GenSymSimple Int32 Int32 where genSymSimpleFresh = return

instance GenSymSimple Int64 Int64 where genSymSimpleFresh = return

instance GenSymSimple Word Word where genSymSimpleFresh = return

instance GenSymSimple Word8 Word8 where genSymSimpleFresh = return

instance GenSymSimple Word16 Word16 where genSymSimpleFresh = return

instance GenSymSimple Word32 Word32 where genSymSimpleFresh = return

instance GenSymSimple Word64 Word64 where genSymSimpleFresh = return

instance GenSymSimple B.ByteString B.ByteString where genSymSimpleFresh = return

-- Bool
instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool () Bool where
  ggenSymFresh = derivedNoSpecGGenSymFresh

-- Enums

-- | Specification for enum values with upper bound (exclusive). The result would chosen from [0 .. upperbound].
--
-- >>> runGenSymFresh (ggenSymFresh (EnumGenUpperBound @Integer 4)) "c" :: UnionM Integer
-- UMrg (If c@0 (Single 0) (If c@1 (Single 1) (If c@2 (Single 2) (Single 3))))
newtype EnumGenUpperBound a = EnumGenUpperBound a

instance (SymBoolOp bool, GenSymSimple () bool, Enum v, GMergeable bool v) => GGenSym bool (EnumGenUpperBound v) v where
  ggenSymFresh (EnumGenUpperBound u) = gchooseFresh (toEnum <$> [0 .. fromEnum u - 1])

-- | Specification for numbers with lower bound (inclusive) and upper bound (exclusive)
--
-- >>> runGenSymFresh (ggenSymFresh (EnumGenBound @Integer 0 4)) "c" :: UnionM Integer
-- UMrg (If c@0 (Single 0) (If c@1 (Single 1) (If c@2 (Single 2) (Single 3))))
data EnumGenBound a = EnumGenBound a a

instance (SymBoolOp bool, GenSymSimple () bool, Enum v, GMergeable bool v) => GGenSym bool (EnumGenBound v) v where
  ggenSymFresh (EnumGenBound l u) = gchooseFresh (toEnum <$> [fromEnum l .. fromEnum u - 1])

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
  genSymSimpleFresh = derivedSameShapeGenSymSimpleFresh

instance
  (SymBoolOp bool, GenSymSimple () bool, GGenSym bool () a, GMergeable bool a, GGenSym bool () b, GMergeable bool b) =>
  GGenSym bool () (Either a b)
  where
  ggenSymFresh = derivedNoSpecGGenSymFresh

-- Maybe
instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple a a, GMergeable bool a) =>
  GGenSym bool (Maybe a) (Maybe a)

instance
  (GenSymSimple a a) =>
  GenSymSimple (Maybe a) (Maybe a)
  where
  genSymSimpleFresh = derivedSameShapeGenSymSimpleFresh

instance (SymBoolOp bool, GenSymSimple () bool, GGenSym bool () a, GMergeable bool a) => GGenSym bool () (Maybe a) where
  ggenSymFresh = derivedNoSpecGGenSymFresh

-- List
instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple () a, GMergeable bool a) =>
  GGenSym bool Integer [a]
  where
  ggenSymFresh v = do
    l <- gl v
    let xs = reverse $ scanr (:) [] l
    gchooseFresh xs
    where
      gl :: (MonadGenSymFresh m) => Integer -> m [a]
      gl v1
        | v1 <= 0 = return []
        | otherwise = do
            l <- genSymSimpleFresh ()
            r <- gl (v1 - 1)
            return $ l : r

-- | Specification for list generation.
--
-- >>> runGenSymFresh (ggenSymFresh (ListSpec 0 2 ())) "c" :: UnionM [SymBool]
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
  ggenSymFresh (ListSpec minLen maxLen subSpec) =
    if minLen < 0 || maxLen < 0 || minLen >= maxLen
      then error $ "Bad lengthes: " ++ show (minLen, maxLen)
      else do
        l <- gl maxLen
        let xs = drop minLen $ reverse $ scanr (:) [] l
        gchooseFresh xs
    where
      gl :: (MonadGenSymFresh m) => Int -> m [a]
      gl currLen
        | currLen <= 0 = return []
        | otherwise = do
            l <- genSymSimpleFresh subSpec
            r <- gl (currLen - 1)
            return $ l : r

instance
  (SymBoolOp bool, GenSymSimple () bool, GenSymSimple a a, GMergeable bool a) =>
  GGenSym bool [a] [a]

instance
  (GenSymSimple a a) =>
  GenSymSimple [a] [a]
  where
  genSymSimpleFresh = derivedSameShapeGenSymSimpleFresh

-- | Specification for list generation of a specific length.
--
-- >>> runGenSymFresh (genSymSimpleFresh (SimpleListSpec 2 ())) "c" :: [SymBool]
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
  ggenSymFresh = fmap mrgSingle . genSymSimpleFresh

instance
  (GenSymSimple spec a) =>
  GenSymSimple (SimpleListSpec spec) [a]
  where
  genSymSimpleFresh (SimpleListSpec len subSpec) =
    if len < 0
      then error $ "Bad lengthes: " ++ show len
      else do
        gl len
    where
      gl :: (MonadGenSymFresh m) => Int -> m [a]
      gl currLen
        | currLen <= 0 = return []
        | otherwise = do
            l <- genSymSimpleFresh subSpec
            r <- gl (currLen - 1)
            return $ l : r

-- ()
instance (SymBoolOp bool, GenSymSimple () bool) => GGenSym bool () ()

instance GenSymSimple () () where
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh (aspec, bspec) = do
    a1 <- ggenSymFresh aspec
    b1 <- ggenSymFresh bspec
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
  genSymSimpleFresh (aspec, bspec) = do
    (,)
      <$> genSymSimpleFresh aspec
      <*> genSymSimpleFresh bspec

instance
  (SymBoolOp bool, GenSymSimple () bool, GGenSym bool () a, GMergeable bool a, GGenSym bool () b, GMergeable bool b) =>
  GGenSym bool () (a, b)
  where
  ggenSymFresh = derivedNoSpecGGenSymFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b
  ) =>
  GenSymSimple () (a, b)
  where
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh (aspec, bspec, cspec) = do
    a1 <- ggenSymFresh aspec
    b1 <- ggenSymFresh bspec
    c1 <- ggenSymFresh cspec
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
  genSymSimpleFresh (aspec, bspec, cspec) = do
    (,,)
      <$> genSymSimpleFresh aspec
      <*> genSymSimpleFresh bspec
      <*> genSymSimpleFresh cspec

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
  ggenSymFresh = derivedNoSpecGGenSymFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c
  ) =>
  GenSymSimple () (a, b, c)
  where
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh (aspec, bspec, cspec, dspec) = do
    a1 <- ggenSymFresh aspec
    b1 <- ggenSymFresh bspec
    c1 <- ggenSymFresh cspec
    d1 <- ggenSymFresh dspec
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
  genSymSimpleFresh (aspec, bspec, cspec, dspec) = do
    (,,,)
      <$> genSymSimpleFresh aspec
      <*> genSymSimpleFresh bspec
      <*> genSymSimpleFresh cspec
      <*> genSymSimpleFresh dspec

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
  ggenSymFresh = derivedNoSpecGGenSymFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c,
    GenSymSimple () d
  ) =>
  GenSymSimple () (a, b, c, d)
  where
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh (aspec, bspec, cspec, dspec, espec) = do
    a1 <- ggenSymFresh aspec
    b1 <- ggenSymFresh bspec
    c1 <- ggenSymFresh cspec
    d1 <- ggenSymFresh dspec
    e1 <- ggenSymFresh espec
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
  genSymSimpleFresh (aspec, bspec, cspec, dspec, espec) = do
    (,,,,)
      <$> genSymSimpleFresh aspec
      <*> genSymSimpleFresh bspec
      <*> genSymSimpleFresh cspec
      <*> genSymSimpleFresh dspec
      <*> genSymSimpleFresh espec

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
  ggenSymFresh = derivedNoSpecGGenSymFresh

instance
  ( GenSymSimple () a,
    GenSymSimple () b,
    GenSymSimple () c,
    GenSymSimple () d,
    GenSymSimple () e
  ) =>
  GenSymSimple () (a, b, c, d, e)
  where
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh (aspec, bspec, cspec, dspec, espec, fspec) = do
    a1 <- ggenSymFresh aspec
    b1 <- ggenSymFresh bspec
    c1 <- ggenSymFresh cspec
    d1 <- ggenSymFresh dspec
    e1 <- ggenSymFresh espec
    f1 <- ggenSymFresh fspec
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
  genSymSimpleFresh (aspec, bspec, cspec, dspec, espec, fspec) = do
    (,,,,,)
      <$> genSymSimpleFresh aspec
      <*> genSymSimpleFresh bspec
      <*> genSymSimpleFresh cspec
      <*> genSymSimpleFresh dspec
      <*> genSymSimpleFresh espec
      <*> genSymSimpleFresh fspec

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
  ggenSymFresh = derivedNoSpecGGenSymFresh

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
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh (aspec, bspec, cspec, dspec, espec, fspec, gspec) = do
    a1 <- ggenSymFresh aspec
    b1 <- ggenSymFresh bspec
    c1 <- ggenSymFresh cspec
    d1 <- ggenSymFresh dspec
    e1 <- ggenSymFresh espec
    f1 <- ggenSymFresh fspec
    g1 <- ggenSymFresh gspec
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
  genSymSimpleFresh (aspec, bspec, cspec, dspec, espec, fspec, gspec) = do
    (,,,,,,)
      <$> genSymSimpleFresh aspec
      <*> genSymSimpleFresh bspec
      <*> genSymSimpleFresh cspec
      <*> genSymSimpleFresh dspec
      <*> genSymSimpleFresh espec
      <*> genSymSimpleFresh fspec
      <*> genSymSimpleFresh gspec

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
  ggenSymFresh = derivedNoSpecGGenSymFresh

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
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) = do
    a1 <- ggenSymFresh aspec
    b1 <- ggenSymFresh bspec
    c1 <- ggenSymFresh cspec
    d1 <- ggenSymFresh dspec
    e1 <- ggenSymFresh espec
    f1 <- ggenSymFresh fspec
    g1 <- ggenSymFresh gspec
    h1 <- ggenSymFresh hspec
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
  genSymSimpleFresh (aspec, bspec, cspec, dspec, espec, fspec, gspec, hspec) = do
    (,,,,,,,)
      <$> genSymSimpleFresh aspec
      <*> genSymSimpleFresh bspec
      <*> genSymSimpleFresh cspec
      <*> genSymSimpleFresh dspec
      <*> genSymSimpleFresh espec
      <*> genSymSimpleFresh fspec
      <*> genSymSimpleFresh gspec
      <*> genSymSimpleFresh hspec

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
  ggenSymFresh = derivedNoSpecGGenSymFresh

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
  genSymSimpleFresh = derivedNoSpecGenSymSimpleFresh

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
  ggenSymFresh v = do
    x <- ggenSymFresh v
    return $ merge . fmap MaybeT $ x

instance
  {-# OVERLAPPABLE #-}
  ( GenSymSimple spec (m (Maybe a))
  ) =>
  GenSymSimple spec (MaybeT m a)
  where
  genSymSimpleFresh v = MaybeT <$> genSymSimpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (Maybe a)) (m (Maybe a))
  ) =>
  GenSymSimple (MaybeT m a) (MaybeT m a)
  where
  genSymSimpleFresh (MaybeT v) = MaybeT <$> genSymSimpleFresh v

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
  ggenSymFresh v = do
    x <- ggenSymFresh v
    return $ merge . fmap ExceptT $ x

instance
  {-# OVERLAPPABLE #-}
  ( GenSymSimple spec (m (Either a b))
  ) =>
  GenSymSimple spec (ExceptT a m b)
  where
  genSymSimpleFresh v = ExceptT <$> genSymSimpleFresh v

instance
  {-# OVERLAPPING #-}
  ( GenSymSimple (m (Either e a)) (m (Either e a))
  ) =>
  GenSymSimple (ExceptT e m a) (ExceptT e m a)
  where
  genSymSimpleFresh (ExceptT v) = ExceptT <$> genSymSimpleFresh v

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
