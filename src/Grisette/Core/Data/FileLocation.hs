{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

-- |
-- Module      :   Grisette.Core.Data.FileLocation
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.FileLocation
  ( -- * Symbolic constant generation with location
    nameWithLoc,
    slocsym,
    ilocsym,
  )
where

import qualified Data.Text as T
import Debug.Trace.LocationTH (__LOCATION__)
import Grisette.Core.Data.Class.GenSym (FreshIdent (unFreshIdent), name)
import Grisette.Core.Data.Class.Solvable
  ( Solvable (isym, ssym),
  )
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, liftSplice)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XTemplateHaskell

-- | Identifier with the current location as extra information.
--
-- >>> $$(nameWithLoc "a") -- a sample result could be "a:<interactive>:18:4-18"
-- a[<interactive>:...]
--
-- The uniqueness is ensured for the call to 'nameWithLoc' at different location.
nameWithLoc :: T.Text -> SpliceQ FreshIdent
nameWithLoc s =
  [||
  name $
    s <> "[" <> (T.pack $$(liftSplice $ unsafeTExpCoerce __LOCATION__)) <> "]"
  ||]

-- | Generate simply-named symbolic variables. The file location will be
-- attached to the identifier.
--
-- >>> $$(slocsym "a") :: SymBool
-- a[<interactive>:...]
--
-- Calling 'slocsymb' with the same name at different location will always
-- generate different symbolic constants. Calling 'slocsymb' at the same
-- location for multiple times will generate the same symbolic constants.
--
-- >>> ($$(slocsym "a") :: SymBool) == $$(slocsym "a")
-- False
-- >>> let f _ = $$(slocsym "a") :: SymBool
-- >>> f () == f ()
-- True
slocsym :: (Solvable c s) => T.Text -> SpliceQ s
slocsym nm = [||ssym (unFreshIdent $$(nameWithLoc nm))||]

-- | Generate indexed symbolic variables. The file location will be attached to identifier.
--
-- >>> $$(ilocsym "a" 1) :: SymBool
-- a[<interactive>:...]@1
--
-- Calling 'ilocsymb' with the same name and index at different location will
-- always generate different symbolic constants. Calling 'slocsymb' at the same
-- location for multiple times will generate the same symbolic constants.
ilocsym :: (Solvable c s) => T.Text -> Int -> SpliceQ s
ilocsym nm idx = [||isym (unFreshIdent $$(nameWithLoc nm)) idx||]
