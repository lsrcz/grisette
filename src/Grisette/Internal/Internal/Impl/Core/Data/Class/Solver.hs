{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.Solver
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.Solver () where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import Grisette.Internal.Internal.Decl.Core.Data.Class.PPrint (PPrint)
import Grisette.Internal.Internal.Decl.Core.Data.Class.Solver (SolvingFailure)
import Grisette.Internal.Internal.Impl.Core.Data.Class.PPrint ()
import Grisette.Internal.TH.Derivation.Derive (derive)

-- $setup
-- >>> import Grisette
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend

derive
  [''SolvingFailure]
  [ ''Show,
    ''Eq,
    ''PPrint,
    ''NFData,
    ''Hashable,
    ''Serial
  ]

instance Cereal.Serialize SolvingFailure where
  put = serialize
  get = deserialize

instance Binary.Binary SolvingFailure where
  put = serialize
  get = deserialize
