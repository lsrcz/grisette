{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SafeFromFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SafeFromFP (SafeFromFP (..)) where

import Control.Monad.Error.Class (MonadError (throwError))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConvertible (fromFPOr),
    IEEEFPRoundingMode (rna, rne, rtn, rtp, rtz),
    fpIsInfinite,
    fpIsNaN,
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits (symFpIsInfinite, symFpIsNaN),
  )
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd ((.<), (.>)))
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, mrgSingle, tryMerge)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( ConvertibleBound (convertibleLowerBound, convertibleUpperBound),
    FP,
    FPRoundingMode,
    NotRepresentableFPError
      ( FPOverflowError,
        FPUnderflowError,
        InfError,
        NaNError
      ),
    ValidFP,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)

-- | Safe conversion from floating point numbers that throws exceptions when
-- the result isn't representable by the type.
class
  (MonadError e m, TryMerge m, IEEEFPConvertible a fp fprd) =>
  SafeFromFP e a fp fprd m
  where
  safeFromFP :: fprd -> fp -> m a

instance
  (MonadError NotRepresentableFPError m, TryMerge m, ValidFP eb sb) =>
  SafeFromFP NotRepresentableFPError AlgReal (FP eb sb) FPRoundingMode m
  where
  safeFromFP mode a
    | fpIsInfinite a = tryMerge $ throwError InfError
    | fpIsNaN a = tryMerge $ throwError NaNError
    | otherwise = mrgSingle $ fromFPOr undefined mode a

instance
  (MonadError NotRepresentableFPError m, MonadUnion m, ValidFP eb sb) =>
  SafeFromFP
    NotRepresentableFPError
    SymAlgReal
    (SymFP eb sb)
    SymFPRoundingMode
    m
  where
  safeFromFP mode a =
    mrgIf (symFpIsInfinite a) (tryMerge $ throwError InfError) $
      mrgIf (symFpIsNaN a) (tryMerge $ throwError NaNError) $
        mrgSingle $
          fromFPOr 0 mode a

instance
  (MonadError NotRepresentableFPError m, TryMerge m, ValidFP eb sb) =>
  SafeFromFP NotRepresentableFPError Integer (FP eb sb) FPRoundingMode m
  where
  safeFromFP mode a
    | fpIsInfinite a = tryMerge $ throwError InfError
    | fpIsNaN a = tryMerge $ throwError NaNError
    | otherwise = mrgSingle $ fromFPOr 0 mode a

instance
  (MonadError NotRepresentableFPError m, MonadUnion m, ValidFP eb sb) =>
  SafeFromFP
    NotRepresentableFPError
    SymInteger
    (SymFP eb sb)
    SymFPRoundingMode
    m
  where
  safeFromFP mode a =
    mrgIf (symFpIsInfinite a) (tryMerge $ throwError InfError) $
      mrgIf (symFpIsNaN a) (tryMerge $ throwError NaNError) $
        mrgSingle $
          fromFPOr 0 mode a

instance
  ( MonadError NotRepresentableFPError m,
    TryMerge m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  SafeFromFP NotRepresentableFPError (WordN n) (FP eb sb) FPRoundingMode m
  where
  safeFromFP mode a = do
    p :: Integer <- safeFromFP mode a
    if p < (fromIntegral (minBound :: WordN n))
      then tryMerge $ throwError FPUnderflowError
      else
        if p > (fromIntegral (maxBound :: WordN n))
          then tryMerge $ throwError FPOverflowError
          else mrgSingle $ fromIntegral p

symConvertibleLowerBound ::
  forall conBV symBV n eb sb.
  ( ConvertibleBound conBV,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    Solvable (conBV n) (symBV n)
  ) =>
  symBV n ->
  SymFPRoundingMode ->
  SymFP eb sb
symConvertibleLowerBound _ mode =
  symIte
    (mode .== rne)
    (con $ convertibleLowerBound (undefined :: conBV n) rne)
    $ symIte
      (mode .== rna)
      (con $ convertibleLowerBound (undefined :: conBV n) rna)
    $ symIte
      (mode .== rtp)
      (con $ convertibleLowerBound (undefined :: conBV n) rtp)
    $ symIte
      (mode .== rtn)
      (con $ convertibleLowerBound (undefined :: conBV n) rtn)
      (con $ convertibleLowerBound (undefined :: conBV n) rtz)

symConvertibleUpperBound ::
  forall conBV symBV n eb sb.
  ( ConvertibleBound conBV,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    Solvable (conBV n) (symBV n)
  ) =>
  symBV n ->
  SymFPRoundingMode ->
  SymFP eb sb
symConvertibleUpperBound _ mode =
  symIte
    (mode .== rne)
    (con $ convertibleUpperBound (undefined :: conBV n) rne)
    $ symIte
      (mode .== rna)
      (con $ convertibleUpperBound (undefined :: conBV n) rna)
    $ symIte
      (mode .== rtp)
      (con $ convertibleUpperBound (undefined :: conBV n) rtp)
    $ symIte
      (mode .== rtn)
      (con $ convertibleUpperBound (undefined :: conBV n) rtn)
      (con $ convertibleUpperBound (undefined :: conBV n) rtz)

instance
  ( MonadError NotRepresentableFPError m,
    MonadUnion m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  SafeFromFP NotRepresentableFPError (SymWordN n) (SymFP eb sb) SymFPRoundingMode m
  where
  safeFromFP mode a =
    mrgIf (symFpIsInfinite a) (tryMerge $ throwError InfError)
      $ mrgIf (symFpIsNaN a) (tryMerge $ throwError NaNError)
      $ mrgIf
        (a .< symConvertibleLowerBound (undefined :: SymWordN n) mode)
        (tryMerge $ throwError FPUnderflowError)
      $ mrgIf
        (a .> symConvertibleUpperBound (undefined :: SymWordN n) mode)
        (tryMerge $ throwError FPOverflowError)
      $ mrgSingle
      $ fromFPOr 0 mode a

instance
  ( MonadError NotRepresentableFPError m,
    TryMerge m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  SafeFromFP NotRepresentableFPError (IntN n) (FP eb sb) FPRoundingMode m
  where
  safeFromFP mode a = do
    p :: Integer <- safeFromFP mode a
    if p < (fromIntegral (minBound :: IntN n))
      then tryMerge $ throwError FPUnderflowError
      else
        if p > (fromIntegral (maxBound :: IntN n))
          then tryMerge $ throwError FPOverflowError
          else mrgSingle $ fromIntegral p

instance
  ( MonadError NotRepresentableFPError m,
    MonadUnion m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  SafeFromFP NotRepresentableFPError (SymIntN n) (SymFP eb sb) SymFPRoundingMode m
  where
  safeFromFP mode a =
    mrgIf (symFpIsInfinite a) (tryMerge $ throwError InfError)
      $ mrgIf (symFpIsNaN a) (tryMerge $ throwError NaNError)
      $ mrgIf
        (a .< symConvertibleLowerBound (undefined :: SymIntN n) mode)
        (tryMerge $ throwError FPUnderflowError)
      $ mrgIf
        (a .> symConvertibleUpperBound (undefined :: SymIntN n) mode)
        (tryMerge $ throwError FPOverflowError)
      $ mrgSingle
      $ fromFPOr 0 mode a
