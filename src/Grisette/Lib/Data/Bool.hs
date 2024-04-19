{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Lib.Data.Bool (mrgTrue, mrgFalse) where

import Grisette.Internal.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Internal.Core.TH.MergeConstructor
  ( mkMergeConstructor,
  )

mkMergeConstructor "mrg" ''Bool
