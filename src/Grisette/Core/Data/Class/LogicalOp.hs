module Grisette.Core.Data.Class.LogicalOp
  ( LogicalOp (..),
  )
where

import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalAndTerm,
    pevalImplyTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalXorTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool (SymBool))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Symbolic logical operators for symbolic booleans.
--
-- >>> let t = con True :: SymBool
-- >>> let f = con False :: SymBool
-- >>> let a = "a" :: SymBool
-- >>> let b = "b" :: SymBool
-- >>> t ||~ f
-- true
-- >>> a ||~ t
-- true
-- >>> a ||~ f
-- a
-- >>> a ||~ b
-- (|| a b)
-- >>> t &&~ f
-- false
-- >>> a &&~ t
-- a
-- >>> a &&~ f
-- false
-- >>> a &&~ b
-- (&& a b)
-- >>> nots t
-- false
-- >>> nots f
-- true
-- >>> nots a
-- (! a)
-- >>> t `xors` f
-- true
-- >>> t `xors` t
-- false
-- >>> a `xors` t
-- (! a)
-- >>> a `xors` f
-- a
-- >>> a `xors` b
-- (|| (&& (! a) b) (&& a (! b)))
class LogicalOp b where
  -- | Symbolic disjunction
  (||~) :: b -> b -> b
  a ||~ b = nots $ nots a &&~ nots b
  {-# INLINE (||~) #-}

  infixr 2 ||~

  -- | Symbolic conjunction
  (&&~) :: b -> b -> b
  a &&~ b = nots $ nots a ||~ nots b
  {-# INLINE (&&~) #-}

  infixr 3 &&~

  -- | Symbolic negation
  nots :: b -> b

  -- | Symbolic exclusive disjunction
  xors :: b -> b -> b
  a `xors` b = (a &&~ nots b) ||~ (nots a &&~ b)
  {-# INLINE xors #-}

  -- | Symbolic implication
  implies :: b -> b -> b
  a `implies` b = nots a ||~ b
  {-# INLINE implies #-}

  {-# MINIMAL (||~), nots | (&&~), nots #-}

-- LogicalOp instances
instance LogicalOp Bool where
  (||~) = (||)
  {-# INLINE (||~) #-}
  (&&~) = (&&)
  {-# INLINE (&&~) #-}
  nots = not
  {-# INLINE nots #-}

instance LogicalOp SymBool where
  (SymBool l) ||~ (SymBool r) = SymBool $ pevalOrTerm l r
  (SymBool l) &&~ (SymBool r) = SymBool $ pevalAndTerm l r
  nots (SymBool v) = SymBool $ pevalNotTerm v
  (SymBool l) `xors` (SymBool r) = SymBool $ pevalXorTerm l r
  (SymBool l) `implies` (SymBool r) = SymBool $ pevalImplyTerm l r
