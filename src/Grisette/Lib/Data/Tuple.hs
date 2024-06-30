{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Lib.Data.Tuple
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Tuple
  ( mrgUnit,
    mrgTuple2,
    mrgTuple3,
    mrgTuple4,
    mrgTuple5,
    mrgTuple6,
    mrgTuple7,
    mrgTuple8,
  )
where

import Grisette.Internal.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Internal.TH.MergeConstructor
  ( mkMergeConstructor',
  )

mkMergeConstructor' ["mrgUnit"] ''()
mkMergeConstructor' ["mrgTuple2"] ''(,)
mkMergeConstructor' ["mrgTuple3"] ''(,,)
mkMergeConstructor' ["mrgTuple4"] ''(,,,)
mkMergeConstructor' ["mrgTuple5"] ''(,,,,)
mkMergeConstructor' ["mrgTuple6"] ''(,,,,,)
mkMergeConstructor' ["mrgTuple7"] ''(,,,,,,)
mkMergeConstructor' ["mrgTuple8"] ''(,,,,,,,)
