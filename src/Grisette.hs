-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette
  ( -- | Grisette is a tool for performing symbolic evaluation on programs. With
    -- Grisette, you can construct your own symbolic DSL, and get the symbolic
    -- evaluator for it without the need of manually implementing the symbolic
    -- evaluation algorithms. A brief introduction to symbolic evaluation is
    -- available in the "Grisette.Core" module.
    --
    -- This module exports most of the Grisette core APIs. There are more
    -- lifted library constructs in the submodules of @Grisette.Lib@.
    -- Those modules are not exported here and should be imported explicitly.
    -- For example, to use the lifted "Data.List" functions, you should import
    -- "Grisette.Lib.Data.List" explicitly.
    --
    -- Grisette also provides an experimental API for unifying symbolic and
    -- concrete code to avoid code duplication. This API is exported in the
    -- "Grisette.Unified" module. This module should be imported qualified as
    -- it intentionally uses the same names as the "Grisette" module.
    --
    -- The following shows a typical import list:
    --
    -- > import Grisette
    -- > import Grisette.Lib.Data.List
    -- > import qualified Grisette.Unified as U
    -- > import qualified Grisette.Unified.Lib.Data.List as U
    --
    -- Other highly experimental APIs are exported in the
    -- "Grisette.Experimental" and its submodules. These APIs are not stable,
    -- may be buggy, and does not follow the PVP rules.

    -- * Core modules

    -- | This module exports the core operations for manipulating symbolic
    -- values.
    module Grisette.Core,

    -- * Symbolic primitives

    -- | This module provides the support for symbolic evaluating primitive
    -- types.
    module Grisette.SymPrim,

    -- * Solver backend

    -- | This module provides the interaction with the solver backends.
    module Grisette.Backend,

    -- * Core libraries

    -- | This module exports the core lifted library constructs.
    module Grisette.Lib.Base,

    -- * Utils

    -- | This module exports utility functions for working with size-tagged
    -- types.
    module Grisette.Utils,

    -- * Template Haskell

    -- | This module helps with type class derivation and provides some smart
    -- constructors.
    module Grisette.TH,
  )
where

import Grisette.Backend
import Grisette.Core
import Grisette.Lib.Base
import Grisette.SymPrim
import Grisette.TH
import Grisette.Utils
