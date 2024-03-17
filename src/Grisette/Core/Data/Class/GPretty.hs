{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.GPretty
  ( GPretty (..),
    groupedEnclose,
    condEnclose,
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( C,
    C1,
    Constructor (conFixity, conIsRecord, conName),
    D,
    Fixity (Infix, Prefix),
    Generic (Rep, from),
    K1 (K1),
    M1 (M1),
    S,
    Selector (selName),
    U1 (U1),
    V1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import GHC.TypeLits (KnownNat, type (<=))
import Generics.Deriving (Default (Default, unDefault))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
    prettyPrintTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    type (-~>) (SymGeneralFun),
    type (=~>) (SymTabularFun),
  )

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
  ( (<+>),
    align,
    encloseSep,
    flatAlt,
    group,
    nest,
    vcat,
    viaShow,
    vsep,
    Doc,
    Pretty(pretty),
  )
import Data.Functor.Const (Const)
#else
import Data.Text.Prettyprint.Doc
  ( (<+>),
    align,
    encloseSep,
    flatAlt,
    group,
    nest,
    vcat,
    viaShow,
    vsep,
    Doc,
    Pretty(pretty),
  )
#endif

glist :: [Doc ann] -> Doc ann
glist l
  | null l = "[]"
  | length l == 1 = "[" <> head l <> "]"
  | otherwise = groupedEnclose "[" "]" $ encloseSep "" "" (flatAlt ", " ",") l

class GPretty a where
  gpretty :: a -> Doc ann
  gprettyPrec :: Int -> a -> Doc ann
  gprettyList :: [a] -> Doc ann
  gprettyList = align . glist . map gpretty

  gpretty = gprettyPrec 0
  gprettyPrec _ = gpretty

  {-# MINIMAL gpretty | gprettyPrec #-}

#define GPRETTY_SIMPLE(type) \
instance GPretty type where gprettyPrec = viaShowsPrec showsPrec

instance GPretty Char where
  gpretty = viaShow
  gprettyList v = pretty (fromString v :: T.Text)

#if 1
GPRETTY_SIMPLE(Bool)
GPRETTY_SIMPLE(Integer)
GPRETTY_SIMPLE(Int)
GPRETTY_SIMPLE(Int8)
GPRETTY_SIMPLE(Int16)
GPRETTY_SIMPLE(Int32)
GPRETTY_SIMPLE(Int64)
GPRETTY_SIMPLE(Word)
GPRETTY_SIMPLE(Word8)
GPRETTY_SIMPLE(Word16)
GPRETTY_SIMPLE(Word32)
GPRETTY_SIMPLE(Word64)
#endif

instance GPretty B.ByteString where
  gpretty = pretty . C.unpack

instance GPretty T.Text where
  gpretty = pretty

instance (KnownNat n, 1 <= n) => GPretty (IntN n) where
  gpretty = viaShow

instance (KnownNat n, 1 <= n) => GPretty (WordN n) where
  gpretty = viaShow

-- ()
instance GPretty () where
  gpretty = viaShow

-- Either
deriving via
  (Default (Either a b))
  instance
    (GPretty a, GPretty b) => GPretty (Either a b)

-- Maybe
deriving via
  (Default (Maybe a))
  instance
    (GPretty a) => GPretty (Maybe a)

-- List
instance (GPretty a) => GPretty [a] where
  gpretty = gprettyList

-- (,)
deriving via
  (Default (a, b))
  instance
    (GPretty a, GPretty b) => GPretty (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    (GPretty a, GPretty b, GPretty c) => GPretty (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    ( GPretty a,
      GPretty b,
      GPretty c,
      GPretty d
    ) =>
    GPretty (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    ( GPretty a,
      GPretty b,
      GPretty c,
      GPretty d,
      GPretty e
    ) =>
    GPretty (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( GPretty a,
      GPretty b,
      GPretty c,
      GPretty d,
      GPretty e,
      GPretty f
    ) =>
    GPretty (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( GPretty a,
      GPretty b,
      GPretty c,
      GPretty d,
      GPretty e,
      GPretty f,
      GPretty g
    ) =>
    GPretty (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( GPretty a,
      GPretty b,
      GPretty c,
      GPretty d,
      GPretty e,
      GPretty f,
      GPretty g,
      GPretty h
    ) =>
    GPretty (a, b, c, d, e, f, g, h)

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (GPretty (f a), GPretty (g a)) =>
    GPretty (Sum f g a)

-- MaybeT
instance
  (GPretty (m (Maybe a))) =>
  GPretty (MaybeT m a)
  where
  gprettyPrec _ (MaybeT a) =
    group $
      nest 2 $
        vsep
          [ "MaybeT",
            gprettyPrec 11 a
          ]

-- ExceptT
instance
  (GPretty (m (Either e a))) =>
  GPretty (ExceptT e m a)
  where
  gprettyPrec _ (ExceptT a) =
    group $
      nest 2 $
        vsep
          [ "ExceptT",
            gprettyPrec 11 a
          ]

-- WriterT
instance
  (GPretty (m (a, w))) =>
  GPretty (WriterLazy.WriterT w m a)
  where
  gprettyPrec _ (WriterLazy.WriterT a) =
    group $
      nest 2 $
        vsep
          [ "WriterT",
            gprettyPrec 11 a
          ]

instance
  (GPretty (m (a, w))) =>
  GPretty (WriterStrict.WriterT w m a)
  where
  gprettyPrec _ (WriterStrict.WriterT a) =
    group $
      nest 2 $
        vsep
          [ "WriterT",
            gprettyPrec 11 a
          ]

-- Identity
instance (GPretty a) => GPretty (Identity a) where
  gprettyPrec _ (Identity a) =
    group $
      nest 2 $
        vsep
          [ "Identity",
            gprettyPrec 11 a
          ]

-- IdentityT
instance (GPretty (m a)) => GPretty (IdentityT m a) where
  gprettyPrec _ (IdentityT a) =
    group $
      nest 2 $
        vsep
          [ "IdentityT",
            gprettyPrec 11 a
          ]

-- Const
deriving via
  (Default (Const a b))
  instance
    (GPretty a) => GPretty (Const a b)

-- Prettyprint
#define GPRETTY_SYM_SIMPLE(symtype) \
instance GPretty symtype where \
  gpretty (symtype t) = prettyPrintTerm t

#define GPRETTY_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => GPretty (symtype n) where \
  gpretty (symtype t) = prettyPrintTerm t

#define GPRETTY_SYM_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb)\
  => GPretty (sa op sb) where \
  gpretty (cons t) = prettyPrintTerm t

#if 1
GPRETTY_SYM_SIMPLE(SymBool)
GPRETTY_SYM_SIMPLE(SymInteger)
GPRETTY_SYM_BV(SymIntN)
GPRETTY_SYM_BV(SymWordN)
GPRETTY_SYM_FUN(=~>, SymTabularFun)
GPRETTY_SYM_FUN(-~>, SymGeneralFun)
#endif

instance (Generic a, GPretty' (Rep a)) => GPretty (Default a) where
  gprettyPrec i v = gprettyPrec' Pref i $ from $ unDefault v

data Type = Rec | Tup | Pref | Inf String Int

class GPretty' a where
  gprettyPrec' :: Type -> Int -> a c -> Doc ann
  isNullary :: a c -> Bool
  isNullary = error "generic gpretty (isNullary): unnecessary case"

instance GPretty' V1 where
  gprettyPrec' _ _ x = case x of {}

instance GPretty' U1 where
  gprettyPrec' _ _ U1 = ""
  isNullary _ = True

instance (GPretty c) => GPretty' (K1 i c) where
  gprettyPrec' _ n (K1 a) = gprettyPrec n a
  isNullary _ = False

groupedEnclose :: Doc ann -> Doc ann -> Doc ann -> Doc ann
groupedEnclose l r d = group $ align $ vcat [l <> flatAlt " " "" <> d, r]

condEnclose :: Bool -> Doc ann -> Doc ann -> Doc ann -> Doc ann
condEnclose b = if b then groupedEnclose else const $ const id

instance (GPretty' a, Constructor c) => GPretty' (M1 C c a) where
  gprettyPrec' _ n c@(M1 x) =
    case t of
      Tup ->
        prettyBraces t (gprettyPrec' t 0 x)
      Inf _ m ->
        group $ condEnclose (n > m) "(" ")" $ gprettyPrec' t m x
      _ ->
        if isNullary x
          then pretty (conName c)
          else
            group $
              condEnclose (n > 10) "(" ")" $
                align $
                  nest 2 $
                    vsep
                      [ pretty (conName c),
                        prettyBraces t (gprettyPrec' t 11 x)
                      ]
    where
      prettyBraces :: Type -> Doc ann -> Doc ann
      prettyBraces Rec = groupedEnclose "{" "}"
      prettyBraces Tup = groupedEnclose "(" ")"
      prettyBraces Pref = id
      prettyBraces (Inf _ _) = id
      fixity = conFixity c
      t
        | conIsRecord c = Rec
        | conIsTuple c = Tup
        | otherwise = case fixity of
            Prefix -> Pref
            Infix _ i -> Inf (conName c) i
      conIsTuple :: C1 c f p -> Bool
      conIsTuple y = tupleName (conName y)
        where
          tupleName ('(' : ',' : _) = True
          tupleName _ = False

instance (Selector s, GPretty' a) => GPretty' (M1 S s a) where
  gprettyPrec' t n s@(M1 x)
    | selName s == "" =
        case t of
          Pref -> gprettyPrec' t (n + 1) x
          _ -> gprettyPrec' t (n + 1) x
    | otherwise =
        pretty (selName s) <+> "=" <+> gprettyPrec' t 0 x
  isNullary (M1 x) = isNullary x

instance (GPretty' a) => GPretty' (M1 D d a) where
  gprettyPrec' t n (M1 x) = gprettyPrec' t n x

instance (GPretty' a, GPretty' b) => GPretty' (a :+: b) where
  gprettyPrec' t n (L1 x) = gprettyPrec' t n x
  gprettyPrec' t n (R1 x) = gprettyPrec' t n x

instance (GPretty' a, GPretty' b) => GPretty' (a :*: b) where
  gprettyPrec' t@Rec n (a :*: b) =
    vcat
      [ gprettyPrec' t n a,
        "," <+> gprettyPrec' t n b
      ]
  gprettyPrec' t@(Inf s _) n (a :*: b) =
    align $
      nest 2 $
        vsep
          [ gprettyPrec' t n a,
            pretty s <+> gprettyPrec' t n b
          ]
  gprettyPrec' t@Tup _ (a :*: b) =
    vcat
      [ gprettyPrec' t 0 a,
        "," <> flatAlt " " "" <> gprettyPrec' t 0 b
      ]
  gprettyPrec' t@Pref n (a :*: b) =
    vsep
      [ gprettyPrec' t (n + 1) a,
        gprettyPrec' t (n + 1) b
      ]
  isNullary _ = False

viaShowsPrec :: (Int -> a -> ShowS) -> Int -> a -> Doc ann
viaShowsPrec f n a = pretty (f n a "")
