{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Lib.Data.Bool (mrgTrue, mrgFalse) where

import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Core.TH.MergeConstructor
  ( mkMergeConstructor,
  )

mkMergeConstructor "mrg" ''Bool
