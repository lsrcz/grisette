module Grisette.Core.Data.Class.Bool (LogicalOp (..)) where

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
