{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.SymPrim.SymGeneralFunTests (symGeneralFunTests) where

import Grisette.Internal.Core.Data.Class.ExtractSym (ExtractSym (extractSymMaybe))
import Grisette.Internal.Core.Data.Class.Function (Function ((#)))
import Grisette.Internal.Core.Data.Class.ModelOps
  ( SymbolSetRep (buildSymbolSet),
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Internal.Term (TypedAnySymbol)
import Grisette.Internal.SymPrim.Prim.Model (AnySymbolSet, ConstantSymbolSet)
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

symGeneralFunTests :: Test
symGeneralFunTests =
  testGroup
    "SymGeneralFun"
    [ testCase "ExtractSym" $ do
        let f :: SymInteger -~> SymInteger = "f"
        let a :: SymInteger = "a"
        let fa = f # a
        let anySymbolSet =
              buildSymbolSet
                ( "a" :: TypedAnySymbol Integer,
                  "f" :: TypedAnySymbol (Integer --> Integer)
                ) ::
                AnySymbolSet
        extractSymMaybe fa @?= Just anySymbolSet
        extractSymMaybe fa @?= (Nothing :: Maybe ConstantSymbolSet)
    ]
