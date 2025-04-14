{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.PPrint
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.PPrint () where

import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    V1,
    (:.:) (Comp1),
    type (:*:),
  )
import GHC.Real (ratioPrec, ratioPrec1)
import GHC.TypeLits (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    U1,
    (:+:),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Symbol (Identifier, Symbol)
import Grisette.Internal.Internal.Decl.Core.Data.Class.PPrint
  ( Doc,
    PPrint (pformat, pformatList, pformatPrec),
    PPrint1 (liftPFormatList, liftPFormatPrec),
    PPrint2 (liftPFormatList2, liftPFormatPrec2),
    Pretty (pretty),
    condEnclose,
    pformatListLike,
    pformatPrec1,
    pformatWithConstructor,
    viaShow,
    viaShowsPrec,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Internal.Term (Term)
import Grisette.Internal.SymPrim.Prim.Model
  ( Model (Model),
    SymbolSet (SymbolSet),
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))
import Grisette.Internal.SymPrim.Prim.Term
  ( ModelValue,
    SomeTypedSymbol (SomeTypedSymbol),
    TypedSymbol (unTypedSymbol),
    prettyPrintTerm,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP
  ( SymFP (SymFP),
    SymFPRoundingMode (SymFPRoundingMode),
  )
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>) (SymGeneralFun))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (SymTabularFun))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.Derivation.Derive (derive)

#define FORMAT_SIMPLE(type) \
instance PPrint type where pformatPrec = viaShowsPrec showsPrec

#if 1
FORMAT_SIMPLE(Bool)
FORMAT_SIMPLE(Integer)
FORMAT_SIMPLE(Int)
FORMAT_SIMPLE(Int8)
FORMAT_SIMPLE(Int16)
FORMAT_SIMPLE(Int32)
FORMAT_SIMPLE(Int64)
FORMAT_SIMPLE(Word)
FORMAT_SIMPLE(Word8)
FORMAT_SIMPLE(Word16)
FORMAT_SIMPLE(Word32)
FORMAT_SIMPLE(Word64)
FORMAT_SIMPLE(Float)
FORMAT_SIMPLE(Double)
FORMAT_SIMPLE(FPRoundingMode)
FORMAT_SIMPLE(Monoid.All)
FORMAT_SIMPLE(Monoid.Any)
FORMAT_SIMPLE(Ordering)
FORMAT_SIMPLE(AlgReal)
#endif

instance PPrint (Proxy a) where
  pformatPrec _ _ = "Proxy"
  {-# INLINE pformatPrec #-}

instance PPrint1 Proxy where
  liftPFormatPrec _ _ _ _ = "Proxy"
  {-# INLINE liftPFormatPrec #-}

instance (PPrint a) => PPrint (Ratio a) where
  pformatPrec p r =
    condEnclose (p > ratioPrec) "(" ")" $
      pformatPrec ratioPrec1 (numerator r)
        <> "%"
        <> pformatPrec ratioPrec1 (denominator r)

instance PPrint B.ByteString where
  pformat = pretty . C.unpack

instance PPrint T.Text where
  pformat = pretty

instance (KnownNat n, 1 <= n) => PPrint (IntN n) where
  pformat = viaShow

instance (KnownNat n, 1 <= n) => PPrint (WordN n) where
  pformat = viaShow

instance (ValidFP eb sb) => PPrint (FP eb sb) where
  pformat = viaShow

instance (Show a, Show b) => PPrint (a =-> b) where
  pformat = viaShow

instance PPrint (a --> b) where
  pformat = viaShow

instance PPrint (Term t) where
  pformat = prettyPrintTerm

instance PPrint SomeTerm where
  pformat (SomeTerm t) = prettyPrintTerm t

-- Prettyprint
#define FORMAT_SYM_SIMPLE(symtype) \
instance PPrint symtype where \
  pformat (symtype t) = prettyPrintTerm t

#define FORMAT_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => PPrint (symtype n) where \
  pformat (symtype t) = prettyPrintTerm t

#define FORMAT_SYM_FUN(op, cons) \
instance PPrint (sa op sb) where \
  pformat (cons t) = prettyPrintTerm t

#if 1
FORMAT_SYM_SIMPLE(SymBool)
FORMAT_SYM_SIMPLE(SymInteger)
FORMAT_SYM_SIMPLE(SymFPRoundingMode)
FORMAT_SYM_SIMPLE(SymAlgReal)
FORMAT_SYM_BV(SymIntN)
FORMAT_SYM_BV(SymWordN)
FORMAT_SYM_FUN(=~>, SymTabularFun)
FORMAT_SYM_FUN(-~>, SymGeneralFun)
#endif

instance (ValidFP eb sb) => PPrint (SymFP eb sb) where
  pformat (SymFP t) = prettyPrintTerm t

derive
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError,
    ''ArithException
  ]
  [''PPrint]

derive
  [ ''Either,
    ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,)
  ]
  [''PPrint, ''PPrint1, ''PPrint2]

derive
  [ ''Maybe,
    ''Monoid.Dual,
    ''Monoid.First,
    ''Monoid.Last,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Down,
    ''MaybeT,
    ''ExceptT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT
  ]
  [''PPrint, ''PPrint1]

-- Identity
instance (PPrint a) => PPrint (Identity a) where
  pformatPrec = pformatPrec1

instance PPrint1 Identity where
  liftPFormatPrec f _ n (Identity a) = f n a

-- IdentityT
instance (PPrint1 m, PPrint a) => PPrint (IdentityT m a) where
  pformatPrec = pformatPrec1

instance (PPrint1 m) => PPrint1 (IdentityT m) where
  liftPFormatPrec f l n (IdentityT a) =
    pformatWithConstructor n "IdentityT" [liftPFormatPrec f l 11 a]

-- Product
deriving via
  (Default (Product l r a))
  instance
    (PPrint (l a), PPrint (r a)) => PPrint (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (PPrint1 l, PPrint1 r) => PPrint1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (PPrint (l a), PPrint (r a)) => PPrint (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (PPrint1 l, PPrint1 r) => PPrint1 (Sum l r)

-- Compose
instance (PPrint (f (g a))) => PPrint (Compose f g a) where
  pformatPrec n (Compose a) =
    pformatWithConstructor n "Compose" [pformatPrec 11 a]

instance (PPrint1 f, PPrint1 g) => PPrint1 (Compose f g) where
  liftPFormatPrec f l n (Compose a) =
    pformatWithConstructor
      n
      "Compose"
      [liftPFormatPrec (liftPFormatPrec f l) (liftPFormatList f l) 11 a]

-- Const
deriving via (Default (Const a b)) instance (PPrint a) => PPrint (Const a b)

deriving via (Default1 (Const a)) instance (PPrint a) => PPrint1 (Const a)

-- Alt
deriving via (Default (Alt f a)) instance (PPrint (f a)) => PPrint (Alt f a)

deriving via (Default1 (Alt f)) instance (PPrint1 f) => PPrint1 (Alt f)

-- Ap
deriving via (Default (Ap f a)) instance (PPrint (f a)) => PPrint (Ap f a)

deriving via (Default1 (Ap f)) instance (PPrint1 f) => PPrint1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance PPrint (U1 p)

deriving via (Default (V1 p)) instance PPrint (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (PPrint c) => PPrint (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (PPrint (f p)) => PPrint (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (PPrint (f p), PPrint (g p)) => PPrint ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (PPrint (f p), PPrint (g p)) => PPrint ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (PPrint p) => PPrint (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (PPrint (f p)) => PPrint (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (PPrint (f (g p))) => PPrint ((f :.: g) p)

instance (PPrint a) => PPrint (HS.HashSet a) where
  pformatPrec = pformatPrec1

instance PPrint1 HS.HashSet where
  liftPFormatPrec p l n s =
    pformatWithConstructor n "HashSet" [liftPFormatPrec p l 11 $ HS.toList s]

instance (PPrint k, PPrint v) => PPrint (HM.HashMap k v) where
  pformatPrec = pformatPrec1

instance (PPrint k) => PPrint1 (HM.HashMap k) where
  liftPFormatPrec = liftPFormatPrec2 pformatPrec pformatList

instance PPrint2 HM.HashMap where
  liftPFormatPrec2 pk lk pv lv n s =
    pformatWithConstructor
      n
      "HashMap"
      [ liftPFormatPrec
          (liftPFormatPrec2 pk lk pv lv)
          (liftPFormatList2 pk lk pv lv)
          11
          $ HM.toList s
      ]

instance PPrint Identifier where
  pformat = viaShow

instance PPrint Symbol where
  pformat = viaShow

instance PPrint (TypedSymbol knd t) where
  pformat = viaShow

instance PPrint (SomeTypedSymbol knd) where
  pformat = viaShow

instance PPrint ModelValue where
  pformat = viaShow

instance PPrint Model where
  pformatPrec n (Model m) =
    pformatWithConstructor n "Model" [bodyFormatted]
    where
      pformatSymbolWithoutType :: SomeTypedSymbol knd -> Doc ann
      pformatSymbolWithoutType (SomeTypedSymbol s) = pformat $ unTypedSymbol s
      pformatPair :: (SomeTypedSymbol knd, ModelValue) -> Doc ann
      pformatPair (s, v) = pformatSymbolWithoutType s <> " -> " <> pformat v
      bodyFormatted = pformatListLike "{" "}" $ pformatPair <$> HM.toList m

instance PPrint (SymbolSet knd) where
  pformatPrec n (SymbolSet s) =
    pformatWithConstructor
      n
      "SymbolSet"
      [pformatListLike "{" "}" $ pformat <$> HS.toList s]
