-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Grisette.Experimental
  ( -- * Experimental features

    -- | The experimental features are likely to be changed in the future,
    -- and they do not comply with the semantics versioning policy.
    --
    -- Use the APIs with caution.

    -- ** Symbolic Generation with Errors Class
    GenSymConstrained (..),
    GenSymSimpleConstrained (..),
    genSymConstrained,
    genSymSimpleConstrained,
    derivedSimpleFreshConstrainedNoSpec,
    derivedSimpleFreshConstrainedSameShape,
    derivedFreshConstrainedNoSpec,

    -- ** Some common GenSymConstrained specifications
    SOrdUpperBound (..),
    SOrdLowerBound (..),
    SOrdBound (..),
  )
where

import Grisette.Experimental.GenSymConstrained
  ( GenSymConstrained (..),
    GenSymSimpleConstrained (..),
    SOrdBound (..),
    SOrdLowerBound (..),
    SOrdUpperBound (..),
    derivedFreshConstrainedNoSpec,
    derivedSimpleFreshConstrainedNoSpec,
    derivedSimpleFreshConstrainedSameShape,
    genSymConstrained,
    genSymSimpleConstrained,
  )
