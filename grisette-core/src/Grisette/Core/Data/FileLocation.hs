{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.Data.FileLocation
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.FileLocation
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @grisette-core@ package.
    -- They rely on the implementation in @grisette-symir@ package.

    -- * Symbolic constant generation with location
    FileLocation (..),
    nameWithLoc,
    slocsymb,
    ilocsymb,
  )
where

import Control.DeepSeq
import Data.Hashable
import Debug.Trace.LocationTH (__LOCATION__)
import GHC.Generics
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Solvable
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XTemplateHaskell

-- File location type.
data FileLocation = FileLocation {locPath :: String, locLineno :: Int, locSpan :: (Int, Int)}
  deriving (Eq, Ord, Generic, Lift, NFData, Hashable)

instance Show FileLocation where
  show (FileLocation p l (s1, s2)) = p ++ ":" ++ show l ++ ":" ++ show s1 ++ "-" ++ show s2

parseFileLocation :: String -> FileLocation
parseFileLocation str =
  let r = reverse str
      (s2, r1) = break (== '-') r
      (s1, r2) = break (== ':') $ tail r1
      (l, p) = break (== ':') $ tail r2
   in FileLocation (reverse $ tail p) (read $ reverse l) (read $ reverse s1, read $ reverse s2)

-- | Identifier with the current location as extra information.
--
-- >>> $$(nameWithLoc "a") -- a sample result could be "a:<interactive>:18:4-18"
-- a:<interactive>:...
--
-- The uniqueness is ensured for the call to 'nameWithLoc' at different location.
nameWithLoc :: String -> SpliceQ FreshIdent
nameWithLoc s = [||nameWithInfo s (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]

-- | Generate simply-named symbolic variables. The file location will be
-- attached to the identifier.
--
-- >>> $$(slocsymb "a") :: SymBool
-- a:<interactive>:...
--
-- Calling 'slocsymb' with the same name at different location will always
-- generate different symbolic constants. Calling 'slocsymb' at the same
-- location for multiple times will generate the same symbolic constants.
--
-- >>> ($$(slocsymb "a") :: SymBool) == $$(slocsymb "a")
-- False
-- >>> let f _ = $$(slocsymb "a") :: SymBool
-- >>> f () == f ()
-- True
slocsymb :: (Solvable c s) => String -> SpliceQ s
slocsymb nm = [||sinfosymb nm (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]

-- | Generate indexed symbolic variables. The file location will be attached to identifier.
--
-- >>> $$(ilocsymb "a" 1) :: SymBool
-- a@1:<interactive>:...
--
-- Calling 'ilocsymb' with the same name and index at different location will
-- always generate different symbolic constants. Calling 'slocsymb' at the same
-- location for multiple times will generate the same symbolic constants.
ilocsymb :: (Solvable c s) => String -> Int -> SpliceQ s
ilocsymb nm idx = [||iinfosymb nm idx (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]
