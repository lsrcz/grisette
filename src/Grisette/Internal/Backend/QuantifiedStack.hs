module Grisette.Internal.Backend.QuantifiedStack
  ( QuantifiedSymbols (..),
    QuantifiedStack,
    addQuantified,
    lookupQuantified,
    emptyQuantifiedSymbols,
    addQuantifiedSymbol,
    isQuantifiedSymbol,
    emptyQuantifiedStack,
  )
where

import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import GHC.Stack (HasCallStack)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SomeTypedSymbol,
    SupportedPrim,
    TypedSymbol,
    someTypedSymbol,
  )

newtype QuantifiedSymbols = QuantifiedSymbols
  { _symbols :: S.HashSet SomeTypedSymbol
  }
  deriving (Show)

emptyQuantifiedSymbols :: QuantifiedSymbols
emptyQuantifiedSymbols = QuantifiedSymbols S.empty

addQuantifiedSymbol ::
  TypedSymbol a -> QuantifiedSymbols -> QuantifiedSymbols
addQuantifiedSymbol s (QuantifiedSymbols t) =
  QuantifiedSymbols (S.insert (someTypedSymbol s) t)

isQuantifiedSymbol ::
  (SupportedPrim a) => TypedSymbol a -> QuantifiedSymbols -> Bool
isQuantifiedSymbol s (QuantifiedSymbols t) = S.member (someTypedSymbol s) t

newtype QuantifiedStack = QuantifiedStack
  {_stack :: M.HashMap SomeTypedSymbol Dynamic}

emptyQuantifiedStack :: QuantifiedStack
emptyQuantifiedStack = QuantifiedStack M.empty

addQuantified :: TypedSymbol a -> Dynamic -> QuantifiedStack -> QuantifiedStack
addQuantified s d (QuantifiedStack t) =
  QuantifiedStack (M.insert (someTypedSymbol s) d t)

lookupQuantified :: (HasCallStack) => SomeTypedSymbol -> QuantifiedStack -> Maybe Dynamic
lookupQuantified s (QuantifiedStack t) = M.lookup s t
