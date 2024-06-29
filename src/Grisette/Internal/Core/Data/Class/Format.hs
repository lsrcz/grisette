{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Grisette.Internal.Core.Data.Class.Format
  ( -- * Pretty printing
    Format (..),
    docToTextWith,
    docToTextWithWidth,
    docToText,
    formatTextWith,
    formatTextWithWidth,
    formatText,
    Format1 (..),
    formatPrec1,
    formatList1,
    Format2 (..),
    formatPrec2,
    formatList2,

    -- * Generic 'Format'
    genericFormatPrec,
    genericLiftFormatPrec,
    FormatArgs (..),
    GFormat (..),
    FormatType (..),

    -- * Helpers
    groupedEnclose,
    condEnclose,
    formatWithConstructor,
    formatWithConstructorNoAlign,
    viaShowsPrec,

    -- * Re-exports
    module Prettyprinter,
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
#else
import Data.Text.Prettyprint.Doc as Prettyprinter
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
#endif

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
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( C1,
    Constructor (conFixity, conIsRecord, conName),
    D1,
    Fixity (Infix, Prefix),
    Generic (Rep, from),
    Generic1 (Rep1, from1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1, unPar1),
    Rec1 (Rec1, unRec1),
    S1,
    Selector (selName),
    U1 (U1),
    V1,
    (:.:) (Comp1, unComp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default, unDefault),
    Default1 (Default1, unDefault1),
  )
import Grisette.Internal.Core.Control.Exception (AssertionError, VerificationConditions)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
    prettyPrintTerm,
  )
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
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

prettyPrintList :: [Doc ann] -> Doc ann
prettyPrintList l
  | null l = "[]"
  | length l == 1 = align $ group $ vcat ["[" <> flatAlt " " "" <> head l, "]"]
  | otherwise =
      groupedEnclose "[" "]" . align . vcat $
        ((\v -> v <> flatAlt "," ", ") <$> init l) ++ [last l]

prettyPrintTuple :: [Doc ann] -> Doc ann
prettyPrintTuple l
  | length l >= 2 =
      groupedEnclose "(" ")" . align . vcat $
        ((\v -> v <> flatAlt "," ", ") <$> init l) ++ [last l]
  | otherwise = error "Tuple must have at least 2 elements"

-- | Pretty printing of values.
class Format a where
  format :: a -> Doc ann
  formatPrec :: Int -> a -> Doc ann
  formatList :: [a] -> Doc ann
  formatList = align . prettyPrintList . map format

  format = formatPrec 0
  formatPrec _ = format

  {-# MINIMAL format | formatPrec #-}

instance Format Char where
  format = viaShow
  formatList v = pretty (fromString v :: T.Text)

instance (Format a) => Format [a] where
  format = formatList

instance Format1 [] where
  liftFormatPrec _ l _ = l
  liftFormatList _ l = prettyPrintList . fmap l

-- | Convenience function to layout and render a 'Doc' to 'T.Text'.
--
-- You can control the layout with 'LayoutOptions'.
docToTextWith :: LayoutOptions -> Doc ann -> T.Text
docToTextWith options = renderStrict . layoutPretty options

-- | Convenience function to layout and render a 'Doc' to 'T.Text'.
--
-- You can control the layout with a single number of the width limit.
docToTextWithWidth :: Int -> Doc ann -> T.Text
docToTextWithWidth n
  | n <= 0 = docToTextWith (LayoutOptions Unbounded)
  | otherwise = docToTextWith (LayoutOptions $ AvailablePerLine n 1.0)

-- | Convenience function to layout and render a 'Doc' to 'T.Text'.
--
-- The default layout options 'defaultLayoutOptions' are used.
docToText :: Doc ann -> T.Text
docToText = docToTextWith defaultLayoutOptions

-- | Convenience function to format a value to 'T.Text'.
--
-- You can control the layout with 'LayoutOptions'.
formatTextWith :: (Format a) => LayoutOptions -> a -> T.Text
formatTextWith options = docToTextWith options . format

-- | Convenience function to format a value to 'T.Text'.
--
-- You can control the layout with a single number of the width limit.
formatTextWithWidth :: (Format a) => Int -> a -> T.Text
formatTextWithWidth n = docToTextWithWidth n . format

-- | Convenience function to format a value to 'T.Text'.
--
-- The default layout options 'defaultLayoutOptions' are used.
formatText :: (Format a) => a -> T.Text
formatText = docToText . format

-- | Lifting of the 'Format' class to unary type constructors.
class (forall a. (Format a) => Format (f a)) => Format1 f where
  -- | Lift a pretty-printer to a unary type constructor.
  liftFormatPrec ::
    (Int -> a -> Doc ann) -> ([a] -> Doc ann) -> Int -> f a -> Doc ann

  -- | Lift a pretty-printer to list of values with unary type constructors.
  liftFormatList ::
    (Int -> a -> Doc ann) -> ([a] -> Doc ann) -> [f a] -> Doc ann
  liftFormatList f l = align . prettyPrintList . map (liftFormatPrec f l 0)

-- | Lift the standard pretty-printer ('formatPrec', 'formatList') to unary
-- type constructors.
formatPrec1 :: (Format1 f, Format a) => Int -> f a -> Doc ann
formatPrec1 = liftFormatPrec formatPrec formatList
{-# INLINE formatPrec1 #-}

-- | Lift the standard pretty-printer ('formatPrec', 'formatList') to list of
-- values with unary type constructors.
formatList1 :: (Format1 f, Format a) => [f a] -> Doc ann
formatList1 = liftFormatList formatPrec formatList
{-# INLINE formatList1 #-}

-- | Lifting of the 'Format' class to binary type constructors.
class
  ( forall a. (Format a) => Format1 (f a),
    forall a b. (Format a, Format b) => Format (f a b)
  ) =>
  Format2 f
  where
  -- | Lift two pretty-printers to a binary type constructor.
  liftFormatPrec2 ::
    (Int -> a -> Doc ann) ->
    (Int -> b -> Doc ann) ->
    ([a] -> Doc ann) ->
    ([b] -> Doc ann) ->
    Int ->
    f a b ->
    Doc ann

  -- | Lift two pretty-printers to list of values with binary type constructors.
  liftFormatList2 ::
    (Int -> a -> Doc ann) ->
    (Int -> b -> Doc ann) ->
    ([a] -> Doc ann) ->
    ([b] -> Doc ann) ->
    [f a b] ->
    Doc ann
  liftFormatList2 fa fb la lb =
    align . prettyPrintList . map (liftFormatPrec2 fa fb la lb 0)

-- | Lift the standard pretty-printer ('formatPrec', 'formatList') to binary
-- type constructors.
formatPrec2 :: (Format2 f, Format a, Format b) => Int -> f a b -> Doc ann
formatPrec2 = liftFormatPrec2 formatPrec formatPrec formatList formatList
{-# INLINE formatPrec2 #-}

-- | Lift the standard pretty-printer ('formatPrec', 'formatList') to list of
-- values with binary type constructors.
formatList2 :: (Format2 f, Format a, Format b) => [f a b] -> Doc ann
formatList2 = liftFormatList2 formatPrec formatPrec formatList formatList
{-# INLINE formatList2 #-}

-- | The arguments to the generic 'Format' class.
data family FormatArgs arity a ann :: Type

data instance FormatArgs Arity0 _ _ = FormatArgs0

data instance FormatArgs Arity1 a ann
  = FormatArgs1
      ((Int -> a -> Doc ann))
      (([a] -> Doc ann))

-- | Controls how to pretty-print a generic representation.
data FormatType = Rec | Tup | Pref | Inf String Int
  deriving (Show, Eq)

-- | Enclose a document with left and right documents.
--
-- The pretty printer will try to layout the document in a single line, but the
-- right document may be split to a newline.
groupedEnclose :: Doc ann -> Doc ann -> Doc ann -> Doc ann
groupedEnclose l r d = group $ align $ vcat [l <> flatAlt " " "" <> align d, r]

-- | Conditionally enclose a document with left and right documents.
--
-- If the condition is 'True', then this function is equivalent to
-- 'groupedEnclose'.
condEnclose :: Bool -> Doc ann -> Doc ann -> Doc ann -> Doc ann
condEnclose b = if b then groupedEnclose else const $ const id

-- | Pretty print a list of fields with a constructor.
--
-- Aligns the fields and nests them by 2 spaces.
formatWithConstructor :: Int -> Doc ann -> [Doc ann] -> Doc ann
formatWithConstructor n c l =
  group $ condEnclose (n > 10) "(" ")" $ align $ nest 2 $ vsep (c : l)

-- | Pretty print a list of fields with a constructor without alignment.
formatWithConstructorNoAlign :: Int -> Doc ann -> [Doc ann] -> Doc ann
formatWithConstructorNoAlign n c l =
  group $ condEnclose (n > 10) "(" ")" $ nest 2 $ vsep (c : l)

-- | Pretty print a value using 'showsPrec'.
viaShowsPrec :: (Int -> a -> ShowS) -> Int -> a -> Doc ann
viaShowsPrec f n a = pretty (f n a "")

-- | Generic 'Format' class.
class GFormat arity f where
  gformatPrec :: FormatArgs arity a ann -> FormatType -> Int -> f a -> Doc ann
  gformatList :: (HasCallStack) => FormatArgs arity a ann -> [f a] -> Doc ann
  gformatList = error "generic format (gformatList): unnecessary case"
  gisNullary :: (HasCallStack) => FormatArgs arity a ann -> f a -> Bool
  gisNullary = error "generic format (isNullary): unnecessary case"

instance GFormat arity V1 where
  gformatPrec _ _ _ x = case x of {}

instance GFormat arity U1 where
  gformatPrec _ _ _ U1 = ""
  gisNullary _ _ = True

instance (Format c) => GFormat arity (K1 i c) where
  gformatPrec _ _ n (K1 a) = formatPrec n a
  gisNullary _ _ = False

instance (GFormat arity a, Constructor c) => GFormat arity (C1 c a) where
  gformatPrec arg _ n c@(M1 x) =
    case t of
      Tup ->
        prettyBraces t (gformatPrec arg t 0 x)
      Inf _ m ->
        group $ condEnclose (n > m) "(" ")" $ gformatPrec arg t m x
      _ ->
        if gisNullary arg x
          then format (conName c)
          else
            formatWithConstructorNoAlign
              n
              (format (conName c))
              [prettyBraces t (gformatPrec arg t 11 x)]
    where
      prettyBraces :: FormatType -> Doc ann -> Doc ann
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

instance (Selector s, GFormat arity a) => GFormat arity (S1 s a) where
  gformatPrec arg t n s@(M1 x)
    | selName s == "" =
        case t of
          Pref -> gformatPrec arg t (n + 1) x
          _ -> gformatPrec arg t (n + 1) x
    | otherwise =
        group $
          align $
            nest 2 $
              vsep [pretty (selName s) <+> "=", gformatPrec arg t 0 x]
  gisNullary _ _ = False

instance (GFormat arity a) => GFormat arity (D1 d a) where
  gformatPrec arg _ n (M1 x) = gformatPrec arg Pref n x
  gformatList arg = align . prettyPrintList . fmap (gformatPrec arg Pref 0)

instance (GFormat arity a, GFormat arity b) => GFormat arity (a :+: b) where
  gformatPrec arg t n (L1 x) = gformatPrec arg t n x
  gformatPrec arg t n (R1 x) = gformatPrec arg t n x

instance (GFormat arity a, GFormat arity b) => GFormat arity (a :*: b) where
  gformatPrec arg t@Rec n (a :*: b) =
    align $
      vcat
        [ gformatPrec arg t n a <> "," <> flatAlt "" " ",
          gformatPrec arg t n b
        ]
  gformatPrec arg t@(Inf s _) n (a :*: b) =
    nest 2 $
      vsep
        [ align $ gformatPrec arg t n a,
          pretty s <+> gformatPrec arg t n b
        ]
  gformatPrec arg t@Tup _ (a :*: b) =
    vcat
      [ gformatPrec arg t 0 a <> "," <> flatAlt "" " ",
        gformatPrec arg t 0 b
      ]
  gformatPrec arg t@Pref n (a :*: b) =
    vsep
      [ gformatPrec arg t (n + 1) a,
        gformatPrec arg t (n + 1) b
      ]
  gisNullary _ _ = False

instance GFormat Arity1 Par1 where
  gformatPrec (FormatArgs1 f _) _ n (Par1 a) = f n a
  gformatList (FormatArgs1 _ g) l = g $ unPar1 <$> l

instance (Format1 f) => GFormat Arity1 (Rec1 f) where
  gformatPrec (FormatArgs1 f g) _ n (Rec1 x) = liftFormatPrec f g n x
  gformatList (FormatArgs1 f g) l = liftFormatList f g $ unRec1 <$> l

instance
  (Format1 f, GFormat Arity1 g) =>
  GFormat Arity1 (f :.: g)
  where
  gformatPrec arg t n (Comp1 x) =
    liftFormatPrec (gformatPrec arg t) (gformatList arg) n x
  gformatList arg l =
    liftFormatList (gformatPrec arg Pref) (gformatList arg) $ unComp1 <$> l

genericFormatPrec ::
  (Generic a, GFormat Arity0 (Rep a)) =>
  Int ->
  a ->
  Doc ann
genericFormatPrec n = gformatPrec FormatArgs0 Pref n . from
{-# INLINE genericFormatPrec #-}

genericFormatList ::
  (Generic a, GFormat Arity0 (Rep a)) =>
  [a] ->
  Doc ann
genericFormatList = gformatList FormatArgs0 . fmap from
{-# INLINE genericFormatList #-}

genericLiftFormatPrec ::
  (Generic1 f, GFormat Arity1 (Rep1 f)) =>
  (Int -> a -> Doc ann) ->
  ([a] -> Doc ann) ->
  Int ->
  f a ->
  Doc ann
genericLiftFormatPrec p l n = gformatPrec (FormatArgs1 p l) Pref n . from1
{-# INLINE genericLiftFormatPrec #-}

genericLiftFormatList ::
  (Generic1 f, GFormat Arity1 (Rep1 f)) =>
  (Int -> a -> Doc ann) ->
  ([a] -> Doc ann) ->
  [f a] ->
  Doc ann
genericLiftFormatList p l = gformatList (FormatArgs1 p l) . fmap from1
{-# INLINE genericLiftFormatList #-}

instance
  (Generic a, GFormat Arity0 (Rep a)) =>
  Format (Default a)
  where
  formatPrec n = genericFormatPrec n . unDefault
  formatList = genericFormatList . fmap unDefault

instance
  (Generic1 f, GFormat Arity1 (Rep1 f), Format a) =>
  Format (Default1 f a)
  where
  formatPrec = formatPrec1
  formatList = formatList1

instance
  (Generic1 f, GFormat Arity1 (Rep1 f)) =>
  Format1 (Default1 f)
  where
  liftFormatPrec p l n = genericLiftFormatPrec p l n . unDefault1
  liftFormatList p l = genericLiftFormatList p l . fmap unDefault1

#define FORMAT_SIMPLE(type) \
instance Format type where formatPrec = viaShowsPrec showsPrec

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
#endif

instance Format B.ByteString where
  format = pretty . C.unpack

instance Format T.Text where
  format = pretty

instance (KnownNat n, 1 <= n) => Format (IntN n) where
  format = viaShow

instance (KnownNat n, 1 <= n) => Format (WordN n) where
  format = viaShow

instance (ValidFP eb sb) => Format (FP eb sb) where
  format = viaShow

-- Prettyprint
#define FORMAT_SYM_SIMPLE(symtype) \
instance Format symtype where \
  format (symtype t) = prettyPrintTerm t

#define FORMAT_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => Format (symtype n) where \
  format (symtype t) = prettyPrintTerm t

#define FORMAT_SYM_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb)\
  => Format (sa op sb) where \
  format (cons t) = prettyPrintTerm t

#if 1
FORMAT_SYM_SIMPLE(SymBool)
FORMAT_SYM_SIMPLE(SymInteger)
FORMAT_SYM_SIMPLE(SymFPRoundingMode)
FORMAT_SYM_BV(SymIntN)
FORMAT_SYM_BV(SymWordN)
FORMAT_SYM_FUN(=~>, SymTabularFun)
FORMAT_SYM_FUN(-~>, SymGeneralFun)
#endif

instance (ValidFP eb sb) => Format (SymFP eb sb) where
  format (SymFP t) = prettyPrintTerm t

-- Instance
deriveBuiltins
  (ViaDefault ''Format)
  [''Format]
  [ ''Maybe,
    ''Either,
    ''(),
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
    ''(,,,,,,,,,,,,,,),
    ''AssertionError,
    ''VerificationConditions,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

deriveBuiltins
  (ViaDefault1 ''Format1)
  [''Format, ''Format1]
  [ ''Maybe,
    ''Either,
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
    ''(,,,,,,,,,,,,,,),
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

-- Identity
instance (Format a) => Format (Identity a) where
  formatPrec = formatPrec1

instance Format1 Identity where
  liftFormatPrec f _ n (Identity a) =
    formatWithConstructor n "Identity" [f 11 a]

-- MaybeT
instance
  (Format1 m, Format a) =>
  Format (MaybeT m a)
  where
  formatPrec = formatPrec1

instance
  (Format1 m) =>
  Format1 (MaybeT m)
  where
  liftFormatPrec f l n (MaybeT a) =
    formatWithConstructor
      n
      "MaybeT"
      [liftFormatPrec (liftFormatPrec f l) (liftFormatList f l) 11 a]

-- ExceptT
instance
  (Format1 m, Format e, Format a) =>
  Format (ExceptT e m a)
  where
  formatPrec = formatPrec1

instance
  (Format1 m, Format e) =>
  Format1 (ExceptT e m)
  where
  liftFormatPrec f l n (ExceptT a) =
    formatWithConstructor
      n
      "ExceptT"
      [liftFormatPrec (liftFormatPrec f l) (liftFormatList f l) 11 a]

-- WriterT
instance
  (Format1 m, Format a, Format w) =>
  Format (WriterLazy.WriterT w m a)
  where
  formatPrec = formatPrec1

instance
  (Format1 m, Format w) =>
  Format1 (WriterLazy.WriterT w m)
  where
  liftFormatPrec f l n (WriterLazy.WriterT a) =
    formatWithConstructor
      n
      "WriterT"
      [ liftFormatPrec
          (liftFormatPrec2 f formatPrec l formatList)
          (liftFormatList2 f formatPrec l formatList)
          11
          a
      ]

instance
  (Format1 m, Format a, Format w) =>
  Format (WriterStrict.WriterT w m a)
  where
  formatPrec = formatPrec1

instance
  (Format1 m, Format w) =>
  Format1 (WriterStrict.WriterT w m)
  where
  liftFormatPrec f l n (WriterStrict.WriterT a) =
    formatWithConstructor
      n
      "WriterT"
      [ liftFormatPrec
          (liftFormatPrec2 f formatPrec l formatList)
          (liftFormatList2 f formatPrec l formatList)
          11
          a
      ]

-- IdentityT
instance (Format1 m, Format a) => Format (IdentityT m a) where
  formatPrec = formatPrec1

instance (Format1 m) => Format1 (IdentityT m) where
  liftFormatPrec f l n (IdentityT a) =
    formatWithConstructor n "IdentityT" [liftFormatPrec f l 11 a]

-- Product
deriving via
  (Default (Product l r a))
  instance
    (Format (l a), Format (r a)) => Format (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (Format1 l, Format1 r) => Format1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (Format (l a), Format (r a)) => Format (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (Format1 l, Format1 r) => Format1 (Sum l r)

-- Compose
instance (Format (f (g a))) => Format (Compose f g a) where
  formatPrec n (Compose a) =
    formatWithConstructor n "Compose" [formatPrec 11 a]

instance (Format1 f, Format1 g) => Format1 (Compose f g) where
  liftFormatPrec f l n (Compose a) =
    formatWithConstructor
      n
      "Compose"
      [liftFormatPrec (liftFormatPrec f l) (liftFormatList f l) 11 a]

-- Const
deriving via (Default (Const a b)) instance (Format a) => Format (Const a b)

deriving via (Default1 (Const a)) instance (Format a) => Format1 (Const a)

-- Alt
deriving via (Default (Alt f a)) instance (Format (f a)) => Format (Alt f a)

deriving via (Default1 (Alt f)) instance (Format1 f) => Format1 (Alt f)

-- Ap
deriving via (Default (Ap f a)) instance (Format (f a)) => Format (Ap f a)

deriving via (Default1 (Ap f)) instance (Format1 f) => Format1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance Format (U1 p)

deriving via (Default (V1 p)) instance Format (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (Format c) => Format (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (Format (f p)) => Format (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (Format (f p), Format (g p)) => Format ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (Format (f p), Format (g p)) => Format ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (Format p) => Format (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (Format (f p)) => Format (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (Format (f (g p))) => Format ((f :.: g) p)

-- Format2
instance Format2 Either where
  liftFormatPrec2 fe _ _ _ n (Left e) =
    formatWithConstructor n "Left" [fe 11 e]
  liftFormatPrec2 _ fa _ _ n (Right a) =
    formatWithConstructor n "Right" [fa 11 a]

instance Format2 (,) where
  liftFormatPrec2 fa fb _ _ _ (a, b) =
    prettyPrintTuple [fa 0 a, fb 0 b]

instance (Format a) => Format2 ((,,) a) where
  liftFormatPrec2 fb fc _ _ _ (a, b, c) =
    prettyPrintTuple [format a, fb 0 b, fc 0 c]

instance (Format a, Format b) => Format2 ((,,,) a b) where
  liftFormatPrec2 fc fd _ _ _ (a, b, c, d) =
    prettyPrintTuple [format a, format b, fc 0 c, fd 0 d]

instance (Format a) => Format (HS.HashSet a) where
  formatPrec n s =
    formatWithConstructor n "fromList" [formatPrec 11 $ HS.toList s]

instance (Format k, Format v) => Format (HM.HashMap k v) where
  formatPrec n s =
    formatWithConstructor n "fromList" [formatPrec 11 $ HM.toList s]
