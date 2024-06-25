module Grisette.Internal.TH.DeriveBuiltin
  ( deriveWithHandlers,
    deriveBuiltin,
    deriveBuiltins,
  )
where

import Control.Monad (when)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (strategyClassName),
  )
import Grisette.Internal.TH.DeriveTypeParamHandler
  ( PrimaryConstraint (PrimaryConstraint),
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

deriveBuiltin :: Strategy -> [Name] -> Name -> Q [Dec]
deriveBuiltin strategy constraints name = do
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
        ( (\cls -> SomeDeriveTypeParamHandler $ PrimaryConstraint cls False)
            <$> constraints
        )
        strategy
        True
        numDrop
        (replicate numParam name)

deriveBuiltins :: Strategy -> [Name] -> [Name] -> Q [Dec]
deriveBuiltins strategy constraints =
  fmap concat . traverse (deriveBuiltin strategy constraints)
