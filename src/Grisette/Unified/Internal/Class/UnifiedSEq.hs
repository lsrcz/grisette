{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.SEq (SEq)
import qualified Grisette.Internal.Core.Data.Class.SEq
import Grisette.Internal.Core.TH.DeriveUnifiedInterface
  ( deriveUnifiedInterfaces,
  )
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

instance (Typeable mode, ValidFP eb sb) => UnifiedSEq mode (FP eb sb) where
  withBaseSEq r = withMode @mode r r
  {-# INLINE withBaseSEq #-}

deriveUnifiedInterfaces
  ''UnifiedSEq
  'withBaseSEq
  [ ''Bool,
    ''Integer,
    ''Char,
    ''Int,
    ''Int8,
    ''Int16,
    ''Int32,
    ''Int64,
    ''Word,
    ''Word8,
    ''Word16,
    ''Word32,
    ''Word64,
    ''Float,
    ''Double,
    ''B.ByteString,
    ''T.Text,
    ''FPRoundingMode,
    ''WordN,
    ''IntN,
    ''[],
    ''Maybe,
    ''Either,
    ''(),
    ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''AssertionError,
    ''VerificationConditions
  ]
