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

import Grisette.Internal.TH.Ctor.SmartConstructor
  ( makeNamedSmartCtor,
  )

makeNamedSmartCtor ["mrgUnit"] ''()
makeNamedSmartCtor ["mrgTuple2"] ''(,)
makeNamedSmartCtor ["mrgTuple3"] ''(,,)
makeNamedSmartCtor ["mrgTuple4"] ''(,,,)
makeNamedSmartCtor ["mrgTuple5"] ''(,,,,)
makeNamedSmartCtor ["mrgTuple6"] ''(,,,,,)
makeNamedSmartCtor ["mrgTuple7"] ''(,,,,,,)
makeNamedSmartCtor ["mrgTuple8"] ''(,,,,,,,)
