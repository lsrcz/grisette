{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.TH.DerivationTest (concreteT, symbolicT) where

import Data.Maybe (fromJust)
import Grisette
  ( Default (Default),
    SymInteger,
    ToCon (toCon),
    ToSym (toSym),
  )
import Grisette.TH (deriveAll)
import Grisette.Unified (EvalModeTag (Con, Sym), GetBool, GetData, GetWordN)

data T mode n a
  = T (GetBool mode) [GetWordN mode n] [a] (GetData mode (T mode n a))
  | TNil

deriveAll ''T

concreteT :: T 'Con 10 Integer
concreteT = toSym (T True [10] [10 :: Integer] TNil :: T 'Con 10 Integer)

symbolicT :: T 'Sym 10 SymInteger
symbolicT = fromJust $ toCon (toSym concreteT :: T 'Sym 10 SymInteger)

newtype X mode = X [GetBool mode]

deriveAll ''X
