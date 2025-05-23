{-# LANGUAGE CPP #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.LogicalOp
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (..),
  )
where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif

import Control.Monad.Identity (Identity)
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey))
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.SymPrim.Prim.Term
  ( pevalAndTerm,
    pevalImplyTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalXorTerm,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Symbolic logical operators for symbolic booleans.
--
-- >>> let t = true :: SymBool
-- >>> let f = false :: SymBool
-- >>> let a = "a" :: SymBool
-- >>> let b = "b" :: SymBool
-- >>> t .|| f
-- true
-- >>> a .|| t
-- true
-- >>> a .|| f
-- a
-- >>> a .|| b
-- (|| a b)
-- >>> t .&& f
-- false
-- >>> a .&& t
-- a
-- >>> a .&& f
-- false
-- >>> a .&& b
-- (&& a b)
-- >>> symNot t
-- false
-- >>> symNot f
-- true
-- >>> symNot a
-- (! a)
-- >>> t `symXor` f
-- true
-- >>> t `symXor` t
-- false
-- >>> a `symXor` t
-- (! a)
-- >>> a `symXor` f
-- a
-- >>> a `symXor` b
-- (|| (&& (! a) b) (&& a (! b)))
class LogicalOp b where
  -- | Constant true
  true :: b
  true = symNot false

  -- | Constant false
  false :: b
  false = symNot true

  -- | Symbolic disjunction
  (.||) :: b -> b -> b
  a .|| b = symNot $ symNot a .&& symNot b
  {-# INLINE (.||) #-}

  infixr 2 .||

  -- | Symbolic conjunction
  (.&&) :: b -> b -> b
  a .&& b = symNot $ symNot a .|| symNot b
  {-# INLINE (.&&) #-}

  infixr 3 .&&

  -- | Symbolic negation
  symNot :: b -> b

  -- | Symbolic exclusive disjunction
  symXor :: b -> b -> b
  a `symXor` b = (a .&& symNot b) .|| (symNot a .&& b)
  {-# INLINE symXor #-}

  -- | Symbolic implication
  symImplies :: b -> b -> b
  a `symImplies` b = symNot a .|| b
  {-# INLINE symImplies #-}

  {-# MINIMAL (true | false), ((.||), symNot | (.&&), symNot) #-}

-- LogicalOp instances
instance LogicalOp Bool where
  true = True
  false = False
  (.||) = (||)
  {-# INLINE (.||) #-}
  (.&&) = (&&)
  {-# INLINE (.&&) #-}
  symNot = not
  {-# INLINE symNot #-}

instance LogicalOp SymBool where
  true = con True
  false = con False
  (SymBool l) .|| (SymBool r) = SymBool $ pevalOrTerm l r
  (SymBool l) .&& (SymBool r) = SymBool $ pevalAndTerm l r
  symNot (SymBool v) = SymBool $ pevalNotTerm v
  (SymBool l) `symXor` (SymBool r) = SymBool $ pevalXorTerm l r
  (SymBool l) `symImplies` (SymBool r) = SymBool $ pevalImplyTerm l r

instance (LogicalOp a) => LogicalOp (Identity a) where
  true = pure true
  false = pure false
  (.||) = liftA2 (.||)
  (.&&) = liftA2 (.&&)
  symNot = fmap symNot
  symXor = liftA2 symXor
  symImplies = liftA2 symImplies

instance (LogicalOp a) => LogicalOp (AsKey a) where
  true = AsKey true
  false = AsKey false
  (AsKey l) .|| (AsKey r) = AsKey $ l .|| r
  (AsKey l) .&& (AsKey r) = AsKey $ l .&& r
  symNot (AsKey v) = AsKey $ symNot v
  (AsKey l) `symXor` (AsKey r) = AsKey $ l `symXor` r
  (AsKey l) `symImplies` (AsKey r) = AsKey $ l `symImplies` r
