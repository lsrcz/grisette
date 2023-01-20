{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.Lib.Data.List
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.List
  ( -- * Symbolic versions of 'Data.List' operations
    (!!~),
    symFilter,
    symTake,
    symDrop,
  )
where

import Control.Exception
import Control.Monad.Except
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.Lib.Control.Monad

-- | Symbolic version of 'Data.List.!!', the result would be merged and
-- propagate the mergeable knowledge.
(!!~) ::
  ( MonadUnion uf,
    MonadError e uf,
    TransformError ArrayException e,
    Mergeable a
  ) =>
  [a] ->
  SymInteger ->
  uf a
l !!~ p = go l p 0
  where
    go [] _ _ = throwError $ transformError (IndexOutOfBounds "!!~")
    go (x : xs) p1 i = mrgIf (p1 ==~ i) (mrgReturn x) (go xs p1 $ i + 1)

-- | Symbolic version of 'Data.List.filter', the result would be merged and
-- propagate the mergeable knowledge.
symFilter :: (MonadUnion u, Mergeable a) => (a -> SymBool) -> [a] -> u [a]
symFilter f = go
  where
    go [] = mrgReturn []
    go (x : xs) = do
      r <- go xs
      mrgIf (f x) (mrgReturn (x : r)) (mrgReturn r)

-- | Symbolic version of 'Data.List.take', the result would be merged and
-- propagate the mergeable knowledge.
symTake :: (MonadUnion u, Mergeable a) => SymInteger -> [a] -> u [a]
symTake _ [] = mrgReturn []
symTake x (v : vs) = mrgIf (x <=~ 0) (mrgReturn []) (mrgFmap (v :) $ symTake (x - 1) vs)

-- | Symbolic version of 'Data.List.drop', the result would be merged and
-- propagate the mergeable knowledge.
symDrop :: (MonadUnion u, Mergeable a) => SymInteger -> [a] -> u [a]
symDrop _ [] = mrgReturn []
symDrop x r@(_ : vs) = mrgIf (x <=~ 0) (mrgReturn r) (symDrop (x - 1) vs)
