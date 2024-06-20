{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedSEq
  ( UnifiedSEq (..),
    (.==),
    (./=),
  )
where

import qualified Data.ByteString as B
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import Data.Type.Bool (If)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SEq (SEq)
import qualified Grisette.Internal.Core.Data.Class.SEq
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Unified.Internal.EvaluationMode
  ( IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.Util (withMode)

(.==) ::
  forall mode a. (Typeable mode, UnifiedSEq mode a) => a -> a -> GetBool mode
(.==) a b =
  withMode @mode
    (withBaseSEq @mode @a $ a == b)
    (withBaseSEq @mode @a $ a Grisette.Internal.Core.Data.Class.SEq..== b)

(./=) ::
  forall mode a. (Typeable mode, UnifiedSEq mode a) => a -> a -> GetBool mode
(./=) a b =
  withMode @mode
    (withBaseSEq @mode @a $ a /= b)
    (withBaseSEq @mode @a $ a Grisette.Internal.Core.Data.Class.SEq../= b)

-- | A class that provides a unified symbolic equality comparison for unified
-- types.
--
-- On all values with 'Eq' instance, the comparisons could return concrete
-- results.
--
-- On all values with 'Grisette.SEq' instance, the comparisons could return
-- symbolic results.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .== b :: GetBool mode
class UnifiedSEq mode a where
  withBaseSEq :: ((If (IsConMode mode) (Eq a) (SEq a)) => r) -> r

instance
  {-# INCOHERENT #-}
  (Typeable mode, If (IsConMode mode) (Eq a) (SEq a)) =>
  UnifiedSEq mode a
  where
  withBaseSEq r = r

#define CONCRETE_UNIFIED_SEQ(type) \
instance (Typeable mode) => UnifiedSEq mode type where \
  withBaseSEq r = withMode @mode r r; \
  {-# INLINE withBaseSEq #-}

#define CONCRETE_UNIFIED_SEQ_BV(type) \
instance (Typeable mode, KnownNat n, 1 <= n) => UnifiedSEq mode (type n) where \
  withBaseSEq r = withMode @mode r r; \
  {-# INLINE withBaseSEq #-}

#if 1
CONCRETE_UNIFIED_SEQ(Bool)
CONCRETE_UNIFIED_SEQ(Integer)
CONCRETE_UNIFIED_SEQ(Char)
CONCRETE_UNIFIED_SEQ(Int)
CONCRETE_UNIFIED_SEQ(Int8)
CONCRETE_UNIFIED_SEQ(Int16)
CONCRETE_UNIFIED_SEQ(Int32)
CONCRETE_UNIFIED_SEQ(Int64)
CONCRETE_UNIFIED_SEQ(Word)
CONCRETE_UNIFIED_SEQ(Word8)
CONCRETE_UNIFIED_SEQ(Word16)
CONCRETE_UNIFIED_SEQ(Word32)
CONCRETE_UNIFIED_SEQ(Word64)
CONCRETE_UNIFIED_SEQ(Float)
CONCRETE_UNIFIED_SEQ(Double)
CONCRETE_UNIFIED_SEQ(B.ByteString)
CONCRETE_UNIFIED_SEQ(T.Text)
CONCRETE_UNIFIED_SEQ(FPRoundingMode)
CONCRETE_UNIFIED_SEQ_BV(WordN)
CONCRETE_UNIFIED_SEQ_BV(IntN)
#endif

instance (Typeable mode, ValidFP eb sb) => UnifiedSEq mode (FP eb sb) where
  withBaseSEq r = withMode @mode r r
  {-# INLINE withBaseSEq #-}

instance (Typeable mode, UnifiedSEq mode a) => UnifiedSEq mode [a] where
  withBaseSEq r =
    withMode @mode
      (withBaseSEq @mode @a r)
      (withBaseSEq @mode @a r)
  {-# INLINE withBaseSEq #-}

instance (Typeable mode, UnifiedSEq mode a) => UnifiedSEq mode (Maybe a) where
  withBaseSEq r =
    withMode @mode
      (withBaseSEq @mode @a r)
      (withBaseSEq @mode @a r)
  {-# INLINE withBaseSEq #-}

instance
  (Typeable mode, UnifiedSEq mode a, UnifiedSEq mode b) =>
  UnifiedSEq mode (Either a b)
  where
  withBaseSEq r =
    withMode @mode
      (withBaseSEq @mode @a $ withBaseSEq @mode @b r)
      (withBaseSEq @mode @a $ withBaseSEq @mode @b r)
  {-# INLINE withBaseSEq #-}

instance (Typeable mode) => UnifiedSEq mode () where
  withBaseSEq r = withMode @mode r r
  {-# INLINE withBaseSEq #-}
