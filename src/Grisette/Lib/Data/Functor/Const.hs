{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Lib.Data.Functor.Const (mrgConst) where

import Data.Functor.Const (Const)
import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Core.TH.MergeConstructor (mkMergeConstructor)

mkMergeConstructor "mrg" ''Const
