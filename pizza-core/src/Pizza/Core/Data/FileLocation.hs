{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Pizza.Core.Data.FileLocation
  ( FileLocation (..),
    nameWithLoc,
    slocsymb,
    ilocsymb,
  )
where

import Control.DeepSeq
import Data.Hashable
import Debug.Trace.LocationTH (__LOCATION__)
import GHC.Generics
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat
import Pizza.Core.Data.Class.GenSym
import Pizza.Core.Data.Class.PrimWrapper

-- $setup
-- >>> import Pizza.Core
-- >>> import Pizza.IR.SymPrim
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
nameWithLoc :: String -> SpliceQ GenSymIdent
nameWithLoc s = [||nameWithInfo s (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]

-- | Generate simply-named symbolic variables. The file location will be attached to identifier.
--
-- >>> $$(slocsymb "a") :: SymBool
-- a:<interactive>:...
--
-- The uniqueness is ensured for the call to 'slocsymb' at different location.
slocsymb :: (PrimWrapper s c) => String -> SpliceQ s
slocsymb nm = [||sinfosymb nm (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]

-- | Generate indexed symbolic variables. The file location will be attached to identifier.
--
-- >>> $$(ilocsymb "a" 1) :: SymBool
-- a@1:<interactive>:...
--
-- The uniqueness is ensured for the call to 'ilocsymb' at different location.
ilocsymb :: (PrimWrapper s c) => String -> Int -> SpliceQ s
ilocsymb nm idx = [||iinfosymb nm idx (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))||]
