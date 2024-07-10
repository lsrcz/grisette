{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.Solvable
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.Solvable
  ( -- * Solvable type interface
    Solvable (..),
    pattern Con,
    slocsym,
    ilocsym,
  )
where

import Data.String (IsString)
import Grisette.Internal.Core.Data.Symbol
  ( Identifier,
    Symbol (IndexedSymbol, SimpleSymbol),
    withLoc,
  )
import Language.Haskell.TH.Syntax.Compat (SpliceQ)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

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

  -- | Generate symbolic constants.
  --
  -- Two symbolic constants with the same symbol are the same symbolic constant,
  -- and will always be assigned with the same value by the solver.
  --
  -- >>> sym "a" :: SymBool
  -- a
  -- >>> (sym "a" :: SymBool) == sym "a"
  -- True
  -- >>> (sym "a" :: SymBool) == sym "b"
  -- False
  -- >>> (sym "a" :: SymBool) .&& sym "a"
  -- a
  -- >>> (sym "a" :: SymBool) == isym "a" 1
  -- False
  sym :: Symbol -> t

  -- | Generate simply-named symbolic constants.
  --
  -- Two symbolic constants with the same identifier are the same symbolic
  -- constant, and will always be assigned with the same value by the solver.
  --
  -- >>> ssym "a" :: SymBool
  -- a
  -- >>> (ssym "a" :: SymBool) == ssym "a"
  -- True
  -- >>> (ssym "a" :: SymBool) == ssym "b"
  -- False
  -- >>> (ssym "a" :: SymBool) .&& ssym "a"
  -- a
  ssym :: Identifier -> t
  ssym = sym . SimpleSymbol

  -- | Generate indexed symbolic constants.
  --
  -- Two symbolic constants with the same identifier but different indices are
  -- not the same symbolic constants.
  --
  -- >>> isym "a" 1 :: SymBool
  -- a@1
  isym :: Identifier -> Int -> t
  isym nm idx = sym $ IndexedSymbol nm idx

-- | Extract the concrete value from a solvable value with 'conView'.
--
-- >>> case con True :: SymBool of Con v -> v
-- True
pattern Con :: (Solvable c t) => c -> t
pattern Con c <-
  (conView -> Just c)
  where
    Con c = con c

-- | Generate simply-named symbolic variables. The file location will be
-- attached to the identifier.
--
-- >>> $$(slocsym "a") :: SymBool
-- a:<interactive>:...
--
-- Calling 'slocsym' with the same name at different location will always
-- generate different symbolic constants. Calling 'slocsym' at the same
-- location for multiple times will generate the same symbolic constants.
--
-- >>> ($$(slocsym "a") :: SymBool) == $$(slocsym "a")
-- False
-- >>> let f _ = $$(slocsym "a") :: SymBool
-- >>> f () == f ()
-- True
slocsym :: (Solvable c s) => Identifier -> SpliceQ s
slocsym nm = [||ssym $$(withLoc nm)||]

-- | Generate indexed symbolic variables. The file location will be attached to
-- the identifier.
--
-- >>> $$(ilocsym "a" 1) :: SymBool
-- a:<interactive>:...@1
--
-- Calling 'ilocsym' with the same name and index at different location will
-- always generate different symbolic constants. Calling 'slocsym' at the same
-- location for multiple times will generate the same symbolic constants.
ilocsym :: (Solvable c s) => Identifier -> Int -> SpliceQ s
ilocsym nm idx = [||isym $$(withLoc nm) idx||]
