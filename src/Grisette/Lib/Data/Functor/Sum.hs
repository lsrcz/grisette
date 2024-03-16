{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Lib.Data.Functor.Sum (mrgInR, mrgInL) where

import Data.Functor.Sum (Sum)
import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Core.TH.MergeConstructor
  ( mkMergeConstructor,
  )

mkMergeConstructor "mrg" ''Sum
