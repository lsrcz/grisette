{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Lib.Base
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Base
  ( -- * Symbolic or mrg* variants for the operations in the base package

    -- ** mrg* variants for operations in "Control.Monad"
    mrgReturnWithStrategy,
    mrgBindWithStrategy,
    mrgReturn,
    (>>=~),
    (>>~),
    mrgFoldM,
    mrgMzero,
    mrgMplus,
    mrgFmap,

    -- ** mrg* variants for operations in "Data.Foldable"
    mrgFoldlM,
    mrgFoldrM,
    mrgTraverse_,
    mrgFor_,
    mrgMapM_,
    mrgForM_,
    mrgSequence_,
    mrgMsum,

    -- ** mrg* variants for operations in "Data.Traversable"
    mrgTraverse,
    mrgSequenceA,
    mrgFor,
    mrgMapM,
    mrgForM,
    mrgSequence,
    (!!~),
    symFilter,
    symTake,
    symDrop,
  )
where

import Grisette.Lib.Control.Monad
import Grisette.Lib.Data.Foldable
import Grisette.Lib.Data.List
import Grisette.Lib.Data.Traversable
