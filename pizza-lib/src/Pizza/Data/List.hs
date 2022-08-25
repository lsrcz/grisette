{-# LANGUAGE FlexibleContexts #-}

module Pizza.Data.List
  ( (!!~),
    symFilter,
    symTake,
    symDrop,
  )
where

import Control.Exception
import Control.Monad.Except
import Pizza.Core.Control.Monad.Union
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Error
import Pizza.Core.Data.Class.Integer
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Control.Monad

(!!~) ::
  ( SymBoolOp bool,
    SymIntegerOp bool integer,
    MonadUnion bool uf,
    MonadError e uf,
    TransformError ArrayException e,
    Mergeable bool a
  ) =>
  [a] ->
  integer ->
  uf a
l !!~ p = go l p 0
  where
    go [] _ _ = throwError $ transformError (IndexOutOfBounds "!!~")
    go (x : xs) p1 i = mrgIf (p1 ==~ i) (mrgReturn x) (go xs p1 $ i + 1)

symFilter :: (SymBoolOp bool, MonadUnion bool u, Mergeable bool a) => (a -> bool) -> [a] -> u [a]
symFilter f = go
  where
    go [] = mrgReturn []
    go (x : xs) = do
      r <- go xs
      mrgIf (f x) (mrgReturn (x : r)) (mrgReturn r)

symTake :: (SymBoolOp bool, MonadUnion bool u, Mergeable bool a, SymIntegerOp bool integer) => integer -> [a] -> u [a]
symTake _ [] = mrgReturn []
symTake x (v : vs) = mrgIf (x <=~ 0) (mrgReturn []) (mrgFmap (v :) $ symTake (x - 1) vs)

symDrop :: (SymBoolOp bool, MonadUnion bool u, Mergeable bool a, SymIntegerOp bool integer) => integer -> [a] -> u [a]
symDrop _ [] = mrgReturn []
symDrop x r@(_ : vs) = mrgIf (x <=~ 0) (mrgReturn r) (symDrop (x - 1) vs)
