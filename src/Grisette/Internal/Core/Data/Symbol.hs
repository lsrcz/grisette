{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Symbol
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Symbol
  ( Identifier (..),
    identifier,
    withInfo,
    withLoc,
    uniqueIdentifier,
    Symbol (..),
    simple,
    indexed,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Typeable (Proxy (Proxy), Typeable, eqT, typeRep, type (:~:) (Refl))
import Debug.Trace.LocationTH (__LOCATION__)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH.Syntax (Lift (liftTyped), unsafeTExpCoerce)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, liftSplice)

-- | Identifier type used for 'Grisette.Core.GenSym'
--
-- The constructor is hidden intentionally.
-- You can construct an identifier by:
--
--   * a raw identifier
--
--     The following two expressions will refer to the same identifier (the
--     solver won't distinguish them and would assign the same value to them).
--     The user may need to use unique names to avoid unintentional identifier
--     collision.
--
--     >>> identifier "a"
--     a
--
--     >>> "a" :: Identifier -- available when OverloadedStrings is enabled
--     a
--
--   * bundle the identifier with some user provided information
--
--     Identifiers created with different name or different additional
--     information will not be the same.
--
--     >>> withInfo "a" (1 :: Int)
--     a:1
--
--   * bundle the calling file location with the identifier to ensure global
--     uniqueness
--
--     Identifiers created at different locations will not be the
--     same. The identifiers created at the same location will be the same.
--
--     >>> $$(withLoc "a") -- a sample result could be "a:<interactive>:18:4-18"
--     a:<interactive>:...
data Identifier where
  Identifier :: T.Text -> Identifier
  IdentifierWithInfo ::
    ( Typeable a,
      Ord a,
      Lift a,
      NFData a,
      Show a,
      Hashable a
    ) =>
    Identifier ->
    a ->
    Identifier

instance Show Identifier where
  show (Identifier i) = T.unpack i
  show (IdentifierWithInfo s i) = show s ++ ":" ++ show i

instance IsString Identifier where
  fromString = Identifier . T.pack

instance Eq Identifier where
  Identifier l == Identifier r = l == r
  IdentifierWithInfo l (linfo :: linfo)
    == IdentifierWithInfo r (rinfo :: rinfo) = case eqT @linfo @rinfo of
      Just Refl -> l == r && linfo == rinfo
      _ -> False
  _ == _ = False

instance Ord Identifier where
  Identifier l <= Identifier r = l <= r
  Identifier _ <= _ = True
  _ <= Identifier _ = False
  IdentifierWithInfo l (linfo :: linfo)
    <= IdentifierWithInfo r (rinfo :: rinfo) =
      l < r
        || ( l == r
               && ( case eqT @linfo @rinfo of
                      Just Refl -> linfo <= rinfo
                      _ -> typeRep (Proxy @linfo) <= typeRep (Proxy @rinfo)
                  )
           )

instance Hashable Identifier where
  hashWithSalt s (Identifier n) = s `hashWithSalt` n
  hashWithSalt s (IdentifierWithInfo n i) = s `hashWithSalt` n `hashWithSalt` i

instance Lift Identifier where
  liftTyped (Identifier n) = [||Identifier n||]
  liftTyped (IdentifierWithInfo n i) = [||IdentifierWithInfo n i||]

instance NFData Identifier where
  rnf (Identifier n) = rnf n
  rnf (IdentifierWithInfo n i) = rnf n `seq` rnf i

-- | Simple identifier.
-- The same identifier refers to the same symbolic variable in the whole
-- program.
--
-- The user may need to use unique identifiers to avoid unintentional identifier
-- collision.
identifier :: T.Text -> Identifier
identifier = Identifier

-- | Identifier with extra information.
--
-- The same identifier with the same information refers to the same symbolic
-- variable in the whole program.
--
-- The user may need to use unique identifiers or additional information to
-- avoid unintentional identifier collision.
withInfo ::
  (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) =>
  Identifier ->
  a ->
  Identifier
withInfo = IdentifierWithInfo

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> :set -XTemplateHaskell

-- File location type.
data FileLocation = FileLocation
  { locPath :: String,
    locLineno :: Int,
    locSpan :: (Int, Int)
  }
  deriving (Eq, Ord, Generic, Lift, NFData, Hashable)

instance Show FileLocation where
  show (FileLocation p l (s1, s2)) =
    p ++ ":" ++ show l ++ ":" ++ show s1 ++ "-" ++ show s2

parseFileLocation :: String -> FileLocation
parseFileLocation str =
  let r = reverse str
      (s2, r1) = break (== '-') r
      (s1, r2) = break (== ':') $ tail r1
      (l, p) = break (== ':') $ tail r2
   in FileLocation
        (reverse $ tail p)
        (read $ reverse l)
        (read $ reverse s1, read $ reverse s2)

-- | Identifier with the current location as extra information.
--
-- >>> $$(withLoc "a") -- a sample result could be "a:<interactive>:18:4-18"
-- a:<interactive>:...
--
-- The uniqueness is ensured for the call to 'identifier' at different location.
withLoc :: Identifier -> SpliceQ Identifier
withLoc s =
  [||
  withInfo
    s
    (parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__))
  ||]

identifierCount :: IORef Int
identifierCount = unsafePerformIO $ newIORef 0
{-# NOINLINE identifierCount #-}

newtype UniqueCount = UniqueCount Int
  deriving newtype (Eq, Ord, NFData, Hashable)
  deriving (Lift)

instance Show UniqueCount where
  show (UniqueCount i) = "unique<" <> show i <> ">"

-- | Get a globally unique identifier within the 'IO' monad.
uniqueIdentifier :: T.Text -> IO Identifier
uniqueIdentifier ident = do
  i <- atomicModifyIORef' identifierCount (\x -> (x + 1, x))
  return $ withInfo (identifier ident) (UniqueCount i)

-- | Symbol types for a symbolic variable.
--
-- The symbols can be indexed with an integer.
data Symbol where
  SimpleSymbol :: Identifier -> Symbol
  IndexedSymbol :: Identifier -> Int -> Symbol
  deriving (Eq, Ord, Generic, Lift, NFData, Hashable)

instance Show Symbol where
  show (SimpleSymbol i) = show i
  show (IndexedSymbol i idx) = show i ++ "@" ++ show idx

instance IsString Symbol where
  fromString = SimpleSymbol . fromString

-- | Create a simple symbol.
simple :: Identifier -> Symbol
simple = SimpleSymbol

-- | Create an indexed symbol.
indexed :: Identifier -> Int -> Symbol
indexed = IndexedSymbol
