-- |
-- Module      :   Grisette.Internal.TH.DeriveBuiltin
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.DeriveBuiltin
  ( deriveBuiltinExtra,
    deriveBuiltin,
    deriveBuiltins,
  )
where

import Control.Monad (when)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (strategyClassName),
  )
import Grisette.Internal.TH.DeriveTypeParamHandler
  ( NatShouldBePositive (NatShouldBePositive),
    PrimaryConstraint (PrimaryConstraint),
    SomeDeriveTypeParamHandler (SomeDeriveTypeParamHandler),
  )
import Grisette.Internal.TH.DeriveWithHandlers
  ( deriveWithHandlers,
  )
import Grisette.Internal.TH.Util
  ( classNumParam,
    classParamKinds,
    kindNumParam,
  )
import Language.Haskell.TH (Dec, Name, Q)

-- | Derive a builtin class for a type, with extra handlers.
deriveBuiltinExtra ::
  [SomeDeriveTypeParamHandler] ->
  Maybe [SomeDeriveTypeParamHandler] ->
  Bool ->
  Strategy ->
  [Name] ->
  Name ->
  Q [Dec]
deriveBuiltinExtra
  extraHandlers
  replacedHandlers
  ignoreBodyConstraints
  strategy
  constraints
  name = do
    let finalCtxName = strategyClassName strategy
    numParam <- classNumParam finalCtxName
    when (numParam == 0) $
      fail "deriveBuiltin: the class must have at least one parameter"
    kinds <- classParamKinds finalCtxName
    case kinds of
      [] ->
        fail $
          "deriveBuiltin: the class must have at least one parameter, bug, "
            <> "should not happen"
      (k : ks) -> do
        when (any (/= k) ks) $
          fail "deriveBuiltin: all parameters must have the same kind"
        constraintNumParams <- mapM classNumParam constraints
        when (any (/= numParam) constraintNumParams) $
          fail $
            "deriveBuiltin: all constraints must have the same number of "
              <> "parameters as the results"
        numDrop <- kindNumParam k
        deriveWithHandlers
          ( extraHandlers ++ case replacedHandlers of
              Just handlers -> handlers
              Nothing ->
                SomeDeriveTypeParamHandler NatShouldBePositive
                  : ( (SomeDeriveTypeParamHandler . flip PrimaryConstraint False)
                        <$> constraints
                    )
          )
          strategy
          ignoreBodyConstraints
          numDrop
          (replicate numParam name)

-- | Derive a builtin class for a type.
deriveBuiltin :: Strategy -> [Name] -> Name -> Q [Dec]
deriveBuiltin = deriveBuiltinExtra [] Nothing True

-- | Derive builtin classes for a list of types.
deriveBuiltins :: Strategy -> [Name] -> [Name] -> Q [Dec]
deriveBuiltins strategy constraints =
  fmap concat . traverse (deriveBuiltin strategy constraints)
