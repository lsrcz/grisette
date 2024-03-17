{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
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

import Data.Functor.Const (Const (Const))
import Data.String (IsString)
import qualified Data.Text as T

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
  -- >>> (ssym "a" :: SymBool) .&& ssym "a"
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

-- | Extract the concrete value from a solvable value with 'conView'.
--
-- >>> case con True :: SymBool of Con v -> v
-- True
pattern Con :: (Solvable c t) => c -> t
pattern Con c <-
  (conView -> Just c)
  where
    Con c = con c

instance (Solvable c t) => Solvable (Const c b) (Const t b) where
  con (Const c) = Const $ con c
  {-# INLINE con #-}
  conView (Const t) = Const <$> conView t
  {-# INLINE conView #-}
  ssym = Const . ssym
  {-# INLINE ssym #-}
  isym symbol = Const . isym symbol
  {-# INLINE isym #-}
