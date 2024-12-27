{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.PPrint
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.PPrint
  ( -- * Pretty printing
    PPrint (..),
    docToTextWith,
    docToTextWithWidth,
    docToText,
    pformatTextWith,
    pformatTextWithWidth,
    pformatText,
    pprint,
    PPrint1 (..),
    pformatPrec1,
    pformatList1,
    PPrint2 (..),
    pformatPrec2,
    pformatList2,

    -- * Generic 'PPrint'
    genericPFormatPrec,
    genericLiftPFormatPrec,
    genericPFormatList,
    genericLiftPFormatList,
    PPrintArgs (..),
    GPPrint (..),
    PPrintType (..),

    -- * Helpers
    groupedEnclose,
    condEnclose,
    pformatWithConstructor,
    pformatWithConstructorNoAlign,
    viaShowsPrec,

    -- * Re-exports
    module Prettyprinter,
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc as Prettyprinter
#endif

import Grisette.Internal.Core.Data.Class.Internal.PPrint
import Grisette.Internal.Core.Data.Class.Internal.Instances.PPrint ()
