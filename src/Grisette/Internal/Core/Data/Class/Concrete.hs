module Grisette.Internal.Core.Data.Class.Concrete (Concrete) where

import Control.Monad.Identity (Identity)
import Data.Bits (And, Iff, Ior, Xor)
import Data.Complex (Complex)
import Data.Functor.Compose (Compose)
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (All, Any, Dual, First, Last, Product, Sum)
import Data.Ord (Down)
import Data.Ratio (Ratio)
import Data.Semigroup (Max, Min)
import Data.Tuple (Solo)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | A tag for concrete types.
--
-- Can be used in code to constrain types to concrete ones.
-- For example, when trying to support hash maps in Grisette, we may want to
-- restrict the key type to be concrete. The 'Concrete' type class is handy.
class Concrete a

instance Concrete Void

instance Concrete All

instance Concrete Any

instance Concrete Int

instance Concrete Int8

instance Concrete Int16

instance Concrete Int32

instance Concrete Int64

instance Concrete Integer

instance Concrete Natural

instance Concrete Word

instance Concrete Word8

instance Concrete Word16

instance Concrete Word32

instance Concrete Word64

instance Concrete Float

instance Concrete Double

instance Concrete Char

instance Concrete Bool

instance (Concrete a) => Concrete (Maybe a)

instance (Concrete a, Concrete b) => Concrete (Either a b)

instance (Concrete a) => Concrete [a]

instance (Concrete a, Concrete b) => Concrete (a, b)

instance (Concrete a, Concrete b, Concrete c) => Concrete (a, b, c)

instance
  (Concrete a, Concrete b, Concrete c, Concrete d) =>
  Concrete (a, b, c, d)

instance
  (Concrete a, Concrete b, Concrete c, Concrete d, Concrete e) =>
  Concrete (a, b, c, d, e)

instance
  ( Concrete a,
    Concrete b,
    Concrete c,
    Concrete d,
    Concrete e,
    Concrete f
  ) =>
  Concrete (a, b, c, d, e, f)

instance
  ( Concrete a,
    Concrete b,
    Concrete c,
    Concrete d,
    Concrete e,
    Concrete f,
    Concrete g
  ) =>
  Concrete (a, b, c, d, e, f, g)

instance
  ( Concrete a,
    Concrete b,
    Concrete c,
    Concrete d,
    Concrete e,
    Concrete f,
    Concrete g,
    Concrete h
  ) =>
  Concrete (a, b, c, d, e, f, g, h)

instance
  ( Concrete a,
    Concrete b,
    Concrete c,
    Concrete d,
    Concrete e,
    Concrete f,
    Concrete g,
    Concrete h,
    Concrete i
  ) =>
  Concrete (a, b, c, d, e, f, g, h, i)

instance
  ( Concrete a,
    Concrete b,
    Concrete c,
    Concrete d,
    Concrete e,
    Concrete f,
    Concrete g,
    Concrete h,
    Concrete i,
    Concrete j
  ) =>
  Concrete (a, b, c, d, e, f, g, h, i, j)

instance
  ( Concrete a,
    Concrete b,
    Concrete c,
    Concrete d,
    Concrete e,
    Concrete f,
    Concrete g,
    Concrete h,
    Concrete i,
    Concrete j,
    Concrete k
  ) =>
  Concrete (a, b, c, d, e, f, g, h, i, j, k)

instance (Concrete a) => Concrete (Complex a)

instance (Concrete a) => Concrete (Ratio a)

instance (Concrete a) => Concrete (First a)

instance (Concrete a) => Concrete (Last a)

instance (Concrete a) => Concrete (Min a)

instance (Concrete a) => Concrete (Max a)

instance (Concrete a) => Concrete (NonEmpty a)

instance (Concrete a) => Concrete (And a)

instance (Concrete a) => Concrete (Iff a)

instance (Concrete a) => Concrete (Ior a)

instance (Concrete a) => Concrete (Xor a)

instance (Concrete a) => Concrete (Identity a)

instance (Concrete a) => Concrete (Down a)

instance (Concrete a) => Concrete (Dual a)

instance (Concrete a) => Concrete (Sum a)

instance (Concrete a) => Concrete (Product a)

instance (Concrete a) => Concrete (Solo a)

instance (Concrete (f a), Concrete (g a)) => Concrete (Functor.Product f g a)

instance (Concrete (f a), Concrete (g a)) => Concrete (Functor.Sum f g a)

instance (Concrete (f (g a))) => Concrete (Compose f g a)
