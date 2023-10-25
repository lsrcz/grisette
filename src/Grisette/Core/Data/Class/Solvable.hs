{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Solvable
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Solvable
  ( -- * Solvable type interface
    Solvable (..),
    pattern Con,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.String (IsString)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( conTerm,
    iinfosymTerm,
    isymTerm,
    sinfosymTerm,
    ssymTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
    Term (ConTerm),
    type (-->),
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim
  ( SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    type (-~>) (SymGeneralFun),
    type (=~>) (SymTabularFun),
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XOverloadedStrings

-- | The class defines the creation and pattern matching of solvable type
-- values.
class (IsString t) => Solvable c t | t -> c where
  -- | Wrap a concrete value in a symbolic value.
  --
  -- >>> con True :: SymBool
  -- true
  con :: c -> t

  -- | Extract the concrete value from a symbolic value.
  --
  -- >>> conView (con True :: SymBool)
  -- Just True
  --
  -- >>> conView (ssym "a" :: SymBool)
  -- Nothing
  conView :: t -> Maybe c

  -- | Generate simply-named symbolic constants.
  --
  -- Two symbolic constants with the same name are the same symbolic constant,
  -- and will always be assigned with the same value by the solver.
  --
  -- >>> ssym "a" :: SymBool
  -- a
  -- >>> (ssym "a" :: SymBool) == ssym "a"
  -- True
  -- >>> (ssym "a" :: SymBool) == ssym "b"
  -- False
  -- >>> (ssym "a" :: SymBool) &&~ ssym "a"
  -- a
  ssym :: T.Text -> t

  -- | Generate indexed symbolic constants.
  --
  -- Two symbolic constants with the same name but different indices are
  -- not the same symbolic constants.
  --
  -- >>> isym "a" 1 :: SymBool
  -- a@1
  isym :: T.Text -> Int -> t

  -- | Generate simply-named symbolic constants with some extra information for
  -- disambiguation.
  --
  -- Two symbolic constants with the same name but different extra information
  -- (including info with different types) are considered to be different.
  --
  -- >>> sinfosym "a" "someInfo" :: SymInteger
  -- a:"someInfo"
  sinfosym :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => T.Text -> a -> t

  -- | Generate indexed symbolic constants with some extra information for
  -- disambiguation.
  --
  -- Two symbolic constants with the same name and index but different extra
  -- information (including info with different types) are considered to be
  -- different.
  --
  -- >>> iinfosym "a" 1 "someInfo" :: SymInteger
  -- a@1:"someInfo"
  iinfosym :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => T.Text -> Int -> a -> t

-- | Extract the concrete value from a solvable value with 'conView'.
--
-- >>> case con True :: SymBool of Con v -> v
-- True
pattern Con :: (Solvable c t) => c -> t
pattern Con c <-
  (conView -> Just c)
  where
    Con c = con c

#define SOLVABLE_SIMPLE(contype, symtype) \
instance Solvable contype symtype where \
  con = symtype . conTerm; \
  ssym = symtype . ssymTerm; \
  isym str i = symtype $ isymTerm str i; \
  sinfosym str info = symtype $ sinfosymTerm str info; \
  iinfosym str i info = symtype $ iinfosymTerm str i info; \
  conView (symtype (ConTerm _ t)) = Just t; \
  conView _ = Nothing

#define SOLVABLE_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => Solvable (contype n) (symtype n) where \
  con = symtype . conTerm; \
  ssym = symtype . ssymTerm; \
  isym str i = symtype $ isymTerm str i; \
  sinfosym str info = symtype $ sinfosymTerm str info; \
  iinfosym str i info = symtype $ iinfosymTerm str i info; \
  conView (symtype (ConTerm _ t)) = Just t; \
  conView _ = Nothing

#define SOLVABLE_FUN(symop, conop, symcons) \
instance \
  (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => \
  Solvable (conop ca cb) (symop sa sb) where \
  con = symcons . conTerm; \
  ssym = symcons . ssymTerm; \
  isym str i = symcons $ isymTerm str i; \
  sinfosym str info = symcons $ sinfosymTerm str info; \
  iinfosym str i info = symcons $ iinfosymTerm str i info; \
  conView (symcons (ConTerm _ t)) = Just t; \
  conView _ = Nothing

#if 1
SOLVABLE_SIMPLE(Bool, SymBool)
SOLVABLE_SIMPLE(Integer, SymInteger)
SOLVABLE_BV(IntN, SymIntN)
SOLVABLE_BV(WordN, SymWordN)
SOLVABLE_FUN((=~>), (=->), SymTabularFun)
SOLVABLE_FUN((-~>), (-->), SymGeneralFun)
#endif
