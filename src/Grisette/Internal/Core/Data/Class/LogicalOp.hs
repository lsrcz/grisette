module Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (..),
  )
where

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
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

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
