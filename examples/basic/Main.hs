--  For explanation the code in this example, please refer to the README.md.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import GHC.Generics
import Grisette

data SymExpr
  = SymConst SymInteger
  | SymInput SymInteger
  | SymAdd (Union SymExpr) (Union SymExpr)
  | SymMul (Union SymExpr) (Union SymExpr)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvalSym) via (Default SymExpr)

-- You may use the following template haskell call to derive everything we need.
-- It will require a long list of extensions though, as it generates some
-- redundant constraints. We will refine this in future releases.

-- deriveAllExcept ''SymExpr [''Ord]

mkMergeConstructor "mrg" ''SymExpr

progSpace :: SymInteger -> SymExpr
progSpace x =
  SymAdd
    (mrgSymInput x)
    (mrgIf "choice" (mrgSymInput x) (mrgSymConst "c"))

interpret :: SymExpr -> SymInteger
interpret (SymConst x) = x
interpret (SymInput x) = x
interpret (SymAdd x y) = interpretSpace x + interpretSpace y
interpret (SymMul x y) = interpretSpace x * interpretSpace y

interpretSpace :: Union SymExpr -> SymInteger
interpretSpace = onUnion interpret

executableSpace :: Integer -> SymInteger
executableSpace = interpret . progSpace . toSym

main :: IO ()
main = do
  Right model <- solve z3 $ executableSpace 2 .== 5
  -- result: SymPlus {SymInput x} {SymConst 3}
  print $ evalSym False model (progSpace "x")
  let synthesizedProgram :: Integer -> Integer =
        evalSymToCon model . executableSpace
  -- result: 13
  print $ synthesizedProgram 10
