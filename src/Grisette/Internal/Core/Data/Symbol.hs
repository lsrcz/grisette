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
    withMetadata,
    withLocation,
    mapMetadata,
    uniqueIdentifier,
    Symbol (..),
    simple,
    indexed,
    symbolIdentifier,
    mapIdentifier,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Serialize (Serialize)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Grisette.Internal.Core.Data.SExpr
  ( SExpr (Atom, List, NumberAtom),
    fileLocation,
    showsSExprWithParens,
  )
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax.Compat (SpliceQ)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

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
--   * bundle the identifier with some user provided metadata
--
--     Identifiers created with different name or different additional
--     information will not be the same.
--
--     >>> withMetadata "a" (NumberAtom 1)
--     a:1
--
--   * bundle the calling file location with the identifier to ensure global
--     uniqueness
--
--     Identifiers created at different locations will not be the
--     same. The identifiers created at the same location will be the same.
--
--     >>> $$(withLocation "a") -- a sample result could be "a:[grisette-file-location <interactive> 18 (4 18)]"
--     a:[grisette-file-location <interactive>...]
data Identifier = Identifier {baseIdent :: T.Text, metadata :: SExpr}
  deriving (Eq, Ord, Generic, Lift)
  deriving anyclass (Hashable, NFData, Serialize)

instance Show Identifier where
  showsPrec _ (Identifier i (List [])) = showString (T.unpack i)
  showsPrec _ (Identifier i metadata) =
    showString (T.unpack i)
      . showString ":"
      . showsSExprWithParens '[' ']' metadata

instance IsString Identifier where
  fromString i = Identifier (T.pack i) $ List []

-- | Simple identifier.
-- The same identifier refers to the same symbolic variable in the whole
-- program.
--
-- The user may need to use unique identifiers to avoid unintentional identifier
-- collision.
identifier :: T.Text -> Identifier
identifier = flip Identifier $ List []

-- | Identifier with extra metadata.
--
-- The same identifier with the same metadata refers to the same symbolic
-- variable in the whole program.
--
-- The user may need to use unique identifiers or additional metadata to
-- avoid unintentional identifier collision.
withMetadata :: T.Text -> SExpr -> Identifier
withMetadata = Identifier

-- | Identifier with the file location.
withLocation :: T.Text -> SpliceQ Identifier
withLocation nm = [||withMetadata nm $$fileLocation||]

-- | Modify the metadata of an identifier.
mapMetadata :: (SExpr -> SExpr) -> Identifier -> Identifier
mapMetadata f (Identifier i m) = Identifier i (f m)

identifierCount :: IORef Int
identifierCount = unsafePerformIO $ newIORef 0
{-# NOINLINE identifierCount #-}

-- | Get a globally unique identifier within the 'IO' monad.
uniqueIdentifier :: T.Text -> IO Identifier
uniqueIdentifier ident = do
  i <- atomicModifyIORef' identifierCount (\x -> (x + 1, x))
  return $
    withMetadata
      ident
      (List [Atom "grisette-unique", NumberAtom $ toInteger i])

-- | Symbol types for a symbolic variable.
--
-- The symbols can be indexed with an integer.
data Symbol where
  SimpleSymbol :: Identifier -> Symbol
  IndexedSymbol :: Identifier -> Int -> Symbol
  deriving (Eq, Ord, Generic, Lift, NFData, Serialize)

instance Hashable Symbol where
  hashWithSalt s (SimpleSymbol i) = hashWithSalt s i
  hashWithSalt s (IndexedSymbol i idx) = s `hashWithSalt` i `hashWithSalt` idx
  {-# INLINE hashWithSalt #-}

-- | Get the identifier of a symbol.
symbolIdentifier :: Symbol -> Identifier
symbolIdentifier (SimpleSymbol i) = i
symbolIdentifier (IndexedSymbol i _) = i

-- | Modify the identifier of a symbol.
mapIdentifier :: (Identifier -> Identifier) -> Symbol -> Symbol
mapIdentifier f (SimpleSymbol i) = SimpleSymbol (f i)
mapIdentifier f (IndexedSymbol i idx) = IndexedSymbol (f i) idx

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
