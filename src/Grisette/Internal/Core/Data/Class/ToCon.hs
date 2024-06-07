{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.ToCon
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.ToCon
  ( -- * Converting to concrete values
    ToCon (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity, runIdentity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic (Rep, from, to), K1 (K1), M1 (M1), U1, V1, type (:*:) ((:*:)), type (:+:) (L1, R1))
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving (Default (Default))
import Generics.Deriving.Instances ()
import Grisette.Internal.Core.Control.Exception (AssertionError, VerificationConditions)
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (conView), pattern Con)
import Grisette.Internal.SymPrim.BV
  ( IntN (IntN),
    WordN (WordN),
  )
import Grisette.Internal.SymPrim.FP (FP, FP32, FP64, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.IntBitwidth (intBitwidthQ)
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFP32, SymFP64, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Convert a symbolic value to concrete value if possible.
class ToCon a b where
  -- | Convert a symbolic value to concrete value if possible.
  -- If the symbolic value cannot be converted to concrete, the result will be 'Nothing'.
  --
  -- >>> toCon (ssym "a" :: SymInteger) :: Maybe Integer
  -- Nothing
  --
  -- >>> toCon (con 1 :: SymInteger) :: Maybe Integer
  -- Just 1
  --
  -- 'toCon' works on complex types too.
  --
  -- >>> toCon ([con 1, con 2] :: [SymInteger]) :: Maybe [Integer]
  -- Just [1,2]
  --
  -- >>> toCon ([con 1, ssym "a"] :: [SymInteger]) :: Maybe [Integer]
  -- Nothing
  toCon :: a -> Maybe b

#define CONCRETE_TOCON(type) \
instance ToCon type type where \
  toCon = Just

#define CONCRETE_TOCON_BV(type) \
instance (KnownNat n, 1 <= n) => ToCon (type n) (type n) where \
  toCon = Just

#if 1
CONCRETE_TOCON(Bool)
CONCRETE_TOCON(Integer)
CONCRETE_TOCON(Char)
CONCRETE_TOCON(Int)
CONCRETE_TOCON(Int8)
CONCRETE_TOCON(Int16)
CONCRETE_TOCON(Int32)
CONCRETE_TOCON(Int64)
CONCRETE_TOCON(Word)
CONCRETE_TOCON(Word8)
CONCRETE_TOCON(Word16)
CONCRETE_TOCON(Word32)
CONCRETE_TOCON(Word64)
CONCRETE_TOCON(Float)
CONCRETE_TOCON(Double)
CONCRETE_TOCON(B.ByteString)
CONCRETE_TOCON(T.Text)
CONCRETE_TOCON_BV(WordN)
CONCRETE_TOCON_BV(IntN)
CONCRETE_TOCON(FPRoundingMode)
#endif

instance (ValidFP eb sb) => ToCon (FP eb sb) (FP eb sb) where
  toCon = Just

-- Unit
instance ToCon () () where
  toCon = Just

-- Either
deriving via (Default (Either e2 a2)) instance (ToCon e1 e2, ToCon a1 a2) => ToCon (Either e1 a1) (Either e2 a2)

-- Maybe
deriving via (Default (Maybe a2)) instance (ToCon a1 a2) => ToCon (Maybe a1) (Maybe a2)

-- List
deriving via (Default [b]) instance (ToCon a b) => ToCon [a] [b]

-- (,)
deriving via (Default (a2, b2)) instance (ToCon a1 a2, ToCon b1 b2) => ToCon (a1, b1) (a2, b2)

-- (,,)
deriving via (Default (a2, b2, c2)) instance (ToCon a1 a2, ToCon b1 b2, ToCon c1 c2) => ToCon (a1, b1, c1) (a2, b2, c2)

-- (,,,)
deriving via
  (Default (a2, b2, c2, d2))
  instance
    (ToCon a1 a2, ToCon b1 b2, ToCon c1 c2, ToCon d1 d2) => ToCon (a1, b1, c1, d1) (a2, b2, c2, d2)

-- (,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2))
  instance
    (ToCon a1 a2, ToCon b1 b2, ToCon c1 c2, ToCon d1 d2, ToCon e1 e2) =>
    ToCon (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)

-- (,,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2, f2))
  instance
    (ToCon a1 a2, ToCon b1 b2, ToCon c1 c2, ToCon d1 d2, ToCon e1 e2, ToCon f1 f2) =>
    ToCon (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)

-- (,,,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2, f2, g2))
  instance
    (ToCon a1 a2, ToCon b1 b2, ToCon c1 c2, ToCon d1 d2, ToCon e1 e2, ToCon f1 f2, ToCon g1 g2) =>
    ToCon (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)

-- (,,,,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2, f2, g2, h2))
  instance
    (ToCon a1 a2, ToCon b1 b2, ToCon c1 c2, ToCon d1 d2, ToCon e1 e2, ToCon f1 f2, ToCon g1 g2, ToCon h1 h2) =>
    ToCon (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)

-- MaybeT
instance
  (ToCon (m1 (Maybe a)) (m2 (Maybe b))) =>
  ToCon (MaybeT m1 a) (MaybeT m2 b)
  where
  toCon (MaybeT v) = MaybeT <$> toCon v

-- ExceptT
instance
  (ToCon (m1 (Either e1 a)) (m2 (Either e2 b))) =>
  ToCon (ExceptT e1 m1 a) (ExceptT e2 m2 b)
  where
  toCon (ExceptT v) = ExceptT <$> toCon v

instance
  (ToCon (m1 (Either e1 a)) (Either e2 b)) =>
  ToCon (ExceptT e1 m1 a) (Either e2 b)
  where
  toCon (ExceptT v) = toCon v

-- Sum
deriving via
  (Default (Sum f1 g1 a1))
  instance
    (ToCon (f a) (f1 a1), ToCon (g a) (g1 a1)) => ToCon (Sum f g a) (Sum f1 g1 a1)

-- WriterT
instance
  (ToCon (m1 (a, s1)) (m2 (b, s2))) =>
  ToCon (WriterLazy.WriterT s1 m1 a) (WriterLazy.WriterT s2 m2 b)
  where
  toCon (WriterLazy.WriterT v) = WriterLazy.WriterT <$> toCon v

instance
  (ToCon (m1 (a, s1)) (m2 (b, s2))) =>
  ToCon (WriterStrict.WriterT s1 m1 a) (WriterStrict.WriterT s2 m2 b)
  where
  toCon (WriterStrict.WriterT v) = WriterStrict.WriterT <$> toCon v

-- Identity
instance (ToCon a b) => ToCon (Identity a) (Identity b) where
  toCon (Identity a) = Identity <$> toCon a

instance ToCon (Identity v) v where
  toCon = Just . runIdentity

instance ToCon v (Identity v) where
  toCon = Just . Identity

-- IdentityT
instance (ToCon (m a) (m1 b)) => ToCon (IdentityT m a) (IdentityT m1 b) where
  toCon (IdentityT a) = IdentityT <$> toCon a

#define TO_CON_SYMID_SIMPLE(symtype) \
instance ToCon symtype symtype where \
  toCon = Just

#define TO_CON_SYMID_BV(symtype) \
instance (KnownNat n, 1 <= n) => ToCon (symtype n) (symtype n) where \
  toCon = Just

#define TO_CON_SYMID_FUN(op) \
instance (SupportedPrim a, SupportedPrim b) => ToCon (a op b) (a op b) where \
  toCon = Just

#if 1
TO_CON_SYMID_SIMPLE(SymBool)
TO_CON_SYMID_SIMPLE(SymInteger)
TO_CON_SYMID_BV(SymIntN)
TO_CON_SYMID_BV(SymWordN)
TO_CON_SYMID_FUN(=~>)
TO_CON_SYMID_FUN(-~>)
TO_CON_SYMID_SIMPLE(SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => ToCon (SymFP eb sb) (SymFP eb sb) where
  toCon = Just

#define TO_CON_FROMSYM_SIMPLE(contype, symtype) \
instance ToCon symtype contype where \
  toCon = conView

#define TO_CON_FROMSYM_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToCon (symtype n) (contype n) where \
  toCon = conView

#define TO_CON_FROMSYM_FUN(conop, symop) \
instance (SupportedPrim (conop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  ToCon (symop sa sb) (conop ca cb) where \
  toCon = conView

#if 1
TO_CON_FROMSYM_SIMPLE(Bool, SymBool)
TO_CON_FROMSYM_SIMPLE(Integer, SymInteger)
TO_CON_FROMSYM_BV(IntN, SymIntN)
TO_CON_FROMSYM_BV(WordN, SymWordN)
TO_CON_FROMSYM_FUN((=->), (=~>))
TO_CON_FROMSYM_FUN((-->), (-~>))
TO_CON_FROMSYM_SIMPLE(FPRoundingMode, SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => ToCon (SymFP eb sb) (FP eb sb) where
  toCon = conView

#define TOCON_MACHINE_INTEGER(sbvw, bvw, n, int) \
instance ToCon (sbvw n) int where \
  toCon (Con (bvw v :: bvw n)) = Just $ fromIntegral v; \
  toCon _ = Nothing

#if 1
TOCON_MACHINE_INTEGER(SymIntN, IntN, 8, Int8)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 16, Int16)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 32, Int32)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 64, Int64)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 8, Word8)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 16, Word16)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 32, Word32)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 64, Word64)
TOCON_MACHINE_INTEGER(SymIntN, IntN, $intBitwidthQ, Int)
TOCON_MACHINE_INTEGER(SymWordN, WordN, $intBitwidthQ, Word)
#endif

instance ToCon SymFP32 Float where
  toCon (Con (fp :: FP32)) = Just $ bitCast fp
  toCon _ = Nothing

instance ToCon SymFP64 Double where
  toCon (Con (fp :: FP64)) = Just $ bitCast fp
  toCon _ = Nothing

deriving via
  (Default AssertionError)
  instance
    ToCon AssertionError AssertionError

deriving via
  (Default VerificationConditions)
  instance
    ToCon VerificationConditions VerificationConditions

-- Derivation of ToCon for generic types
instance (Generic a, Generic b, ToCon' (Rep a) (Rep b)) => ToCon a (Default b) where
  toCon v = fmap (Default . to) $ toCon' $ from v

class ToCon' a b where
  toCon' :: a c -> Maybe (b c)

instance ToCon' U1 U1 where
  toCon' = Just

instance ToCon' V1 V1 where
  toCon' = Just

instance (ToCon a b) => ToCon' (K1 i a) (K1 i b) where
  toCon' (K1 a) = K1 <$> toCon a

instance (ToCon' a b) => ToCon' (M1 i c1 a) (M1 i c2 b) where
  toCon' (M1 a) = M1 <$> toCon' a

instance (ToCon' a1 a2, ToCon' b1 b2) => ToCon' (a1 :+: b1) (a2 :+: b2) where
  toCon' (L1 a) = L1 <$> toCon' a
  toCon' (R1 a) = R1 <$> toCon' a

instance (ToCon' a1 a2, ToCon' b1 b2) => ToCon' (a1 :*: b1) (a2 :*: b2) where
  toCon' (a :*: b) = do
    ac <- toCon' a
    bc <- toCon' b
    return $ ac :*: bc
