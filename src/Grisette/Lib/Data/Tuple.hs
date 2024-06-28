{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
