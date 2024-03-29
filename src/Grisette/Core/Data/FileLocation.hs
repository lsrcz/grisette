{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
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
    FileLocation (..),
    nameWithLoc,
    slocsym,
    ilocsym,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Debug.Trace.LocationTH (__LOCATION__)
import GHC.Generics (Generic)
import Grisette.Core.Data.Class.GenSym (FreshIdent, nameWithInfo)
import Grisette.Core.Data.Class.Solvable
  ( Solvable (iinfosym, sinfosym),
  )
import Language.Haskell.TH.Syntax (Lift, unsafeTExpCoerce)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, liftSplice)

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
nameWithLoc :: T.Text -> SpliceQ FreshIdent
nameWithLoc s = [||nameWithInfo s (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]

-- | Generate simply-named symbolic variables. The file location will be
-- attached to the identifier.
--
-- >>> $$(slocsym "a") :: SymBool
-- a:<interactive>:...
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
slocsym nm = [||sinfosym nm (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]

-- | Generate indexed symbolic variables. The file location will be attached to identifier.
--
-- >>> $$(ilocsym "a" 1) :: SymBool
-- a@1:<interactive>:...
--
-- Calling 'ilocsymb' with the same name and index at different location will
-- always generate different symbolic constants. Calling 'slocsymb' at the same
-- location for multiple times will generate the same symbolic constants.
ilocsym :: (Solvable c s) => T.Text -> Int -> SpliceQ s
ilocsym nm idx = [||iinfosym nm idx (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]
