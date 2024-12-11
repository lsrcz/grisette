{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.TH.DerivationTest (concreteT, symbolicT) where

import Control.Monad.Identity (Identity (Identity))
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Grisette
  ( BasicSymPrim,
    Default (Default),
    EvalSym,
    EvalSym1,
    EvalSym2,
    ExtractSym,
    ExtractSym1,
    ExtractSym2,
    Mergeable,
    Mergeable1,
    Mergeable2,
    SymBool,
    SymInteger,
    ToCon (toCon),
    ToSym (toSym),
    Union,
    deriveAll,
    deriveGADT,
  )
import Grisette.Unified (EvalModeTag (C, S), GetBool, GetData, GetWordN)

data T mode n a
  = T (GetBool mode) [GetWordN mode n] [a] (GetData mode (T mode n a))
  | TNil

deriveAll ''T

concreteT :: T 'C 10 Integer
concreteT =
  toSym (T True [10] [10 :: Integer] (Identity TNil) :: T 'C 10 Integer)

symbolicT :: T 'S 10 SymInteger
symbolicT = fromJust $ toCon (toSym concreteT :: T 'S 10 SymInteger)

newtype X mode = X [GetBool mode]

deriveAll ''X

data IdenticalFields mode n = IdenticalFields
  { a :: n,
    b :: n,
    c :: Maybe Int,
    d :: Maybe Int
  }

deriveAll ''IdenticalFields

data Expr f a where
  I :: SymInteger -> Expr f SymInteger
  B :: SymBool -> Expr f SymBool
  Add :: Union (Expr f SymInteger) -> Union (Expr f SymInteger) -> Expr f SymInteger
  Mul :: Union (Expr f SymInteger) -> Union (Expr f SymInteger) -> Expr f SymInteger
  Eq :: (BasicSymPrim a, Typeable a) => Union (Expr f a) -> Union (Expr f a) -> Expr f SymBool
  Eq3 ::
    (BasicSymPrim a, Typeable b) =>
    Union (Expr f a) ->
    Union (Expr f a) ->
    Union (Expr f b) ->
    Union (Expr f b) ->
    Expr f b
  XExpr :: f a -> Expr f a

deriveGADT
  ''Expr
  [ ''Mergeable,
    ''Mergeable1,
    ''EvalSym,
    ''EvalSym1,
    ''ExtractSym,
    ''ExtractSym1
  ]

data P a b = P a | Q Int

deriveGADT
  ''P
  [ ''Mergeable,
    ''Mergeable1,
    ''Mergeable2,
    ''EvalSym,
    ''EvalSym1,
    ''EvalSym2,
    ''ExtractSym,
    ''ExtractSym1,
    ''ExtractSym2
  ]

