-- |
-- Module      :   Grisette.Internal.TH.Ctor.Common
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Ctor.Common
  ( withNameTransformer,
    prefixTransformer,
    decapitalizeTransformer,
  )
where

import Control.Monad (unless)
import Data.Char (isAlphaNum, toLower)
import Data.Foldable (traverse_)
import Language.Haskell.TH (Dec, Name, Q, nameBase)
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorName),
    DatatypeInfo (datatypeCons),
    reifyDatatype,
  )

checkName :: String -> Q ()
checkName name =
  unless (all (\x -> isAlphaNum x || x == '\'' || x == '_') name) $
    fail
      ( "Constructor name contain invalid characters, consider providing a "
          ++ "custom name: "
          ++ show name
      )

-- | Generate smart constructor given a type name, using a name transformer
-- to transform constructor names.
withNameTransformer ::
  -- | A function that generates decs given a list of constructor names and a
  -- type name
  ([String] -> Name -> Q [Dec]) ->
  -- | A function that transforms constructor names
  (String -> String) ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
withNameTransformer namedGen nameTransformer typName = do
  d <- reifyDatatype typName
  let constructorNames = nameBase . constructorName <$> datatypeCons d
  let transformedNames = nameTransformer <$> constructorNames
  traverse_ checkName transformedNames
  namedGen transformedNames typName

-- | A name transformer that prefixes a string to the constructor name
prefixTransformer :: String -> String -> String
prefixTransformer = (++)

-- | A name transformer that converts the first character to lowercase
decapitalizeTransformer :: String -> String
decapitalizeTransformer (x : xs) = toLower x : xs
decapitalizeTransformer [] = []
