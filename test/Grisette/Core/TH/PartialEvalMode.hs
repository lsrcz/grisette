{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Core.TH.PartialEvalMode
  ( PartialEvalMode,
    MonadPartialEvalMode,
  )
where

import Grisette.Unified
  ( TheoryToUnify (UBool, UFP, UWordN),
    genEvalMode,
  )

genEvalMode "PartialEvalMode" [UBool, UWordN, UFP]
