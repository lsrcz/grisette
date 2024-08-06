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

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.PPrint
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.PPrint
  ( -- * Pretty printing
    PPrint (..),
    docToTextWith,
    docToTextWithWidth,
    docToText,
    pformatTextWith,
    pformatTextWithWidth,
    pformatText,
    pprint,
    PPrint1 (..),
    pformatPrec1,
    pformatList1,
    PPrint2 (..),
    pformatPrec2,
    pformatList2,

    -- * Generic 'PPrint'
    genericPFormatPrec,
    genericLiftPFormatPrec,
    genericPFormatList,
    genericLiftPFormatList,
    PPrintArgs (..),
    GPPrint (..),
    PPrintType (..),

    -- * Helpers
    groupedEnclose,
    condEnclose,
    pformatWithConstructor,
    pformatWithConstructorNoAlign,
    viaShowsPrec,

    -- * Re-exports
    module Prettyprinter,
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Render.Text (renderStrict)
#else
import Data.Text.Prettyprint.Doc as Prettyprinter
import Data.Text.Prettyprint.Doc.Render.String (renderString)
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
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Symbol (Identifier, Symbol)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (BitwidthMismatch, IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, NotRepresentableFPError, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term ()
import Grisette.Internal.SymPrim.Prim.Model
  ( Model (Model),
    SymbolSet (SymbolSet),
  )
import Grisette.Internal.SymPrim.Prim.ModelValue (ModelValue)
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim,
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
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- | Pretty printing of values.
--
-- This class is similar to the 'Pretty' class from the "Prettyprinter" package,
-- but it also provides pretty printing with a given precedence level.
--
-- We are able to derive instances of this class for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving PPrint via (Default X)
--
-- The derived instance will pretty print the value with a format similar to the
-- one used by ormolu.
class PPrint a where
  pformat :: a -> Doc ann
  pformatPrec :: Int -> a -> Doc ann
  pformatList :: [a] -> Doc ann
  pformatList = align . prettyPrintList . map pformat

  pformat = pformatPrec 0
  pformatPrec _ = pformat

  {-# MINIMAL pformat | pformatPrec #-}

pformatListLike :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
pformatListLike ldelim rdelim l
  | null l = ldelim <> rdelim
  | length l == 1 =
      align $ group $ vcat [ldelim <> flatAlt " " "" <> head l, rdelim]
  | otherwise =
      groupedEnclose ldelim rdelim . align . vcat $
        ((\v -> v <> flatAlt "," ", ") <$> init l) ++ [last l]

prettyPrintList :: [Doc ann] -> Doc ann
prettyPrintList = pformatListLike "[" "]"

prettyPrintTuple :: [Doc ann] -> Doc ann
prettyPrintTuple l
  | length l >= 2 =
      groupedEnclose "(" ")" . align . vcat $
        ((\v -> v <> flatAlt "," ", ") <$> init l) ++ [last l]
  | otherwise = error "Tuple must have at least 2 elements"

instance PPrint Char where
  pformat = viaShow
  pformatList v = pretty (fromString v :: T.Text)

instance (PPrint a) => PPrint [a] where
  pformat = pformatList

-- | Convenience function to layout and render a 'Doc' to 'T.Text'.
--
-- You can control the layout with t'LayoutOptions'.
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
-- You can control the layout with t'LayoutOptions'.
pformatTextWith :: (PPrint a) => LayoutOptions -> a -> T.Text
pformatTextWith options = docToTextWith options . pformat

-- | Convenience function to format a value to 'T.Text'.
--
-- You can control the layout with a single number of the width limit.
pformatTextWithWidth :: (PPrint a) => Int -> a -> T.Text
pformatTextWithWidth n = docToTextWithWidth n . pformat

-- | Convenience function to format a value to 'T.Text'.
--
-- The default layout options 'defaultLayoutOptions' are used.
pformatText :: (PPrint a) => a -> T.Text
pformatText = docToText . pformat

-- | Pretty print a value to the standard output.
pprint :: (PPrint a) => a -> IO ()
pprint = putStrLn . renderString . layoutPretty defaultLayoutOptions . pformat

-- | Lifting of the 'PPrint' class to unary type constructors.
class (forall a. (PPrint a) => PPrint (f a)) => PPrint1 f where
  -- | Lift a pretty-printer to a unary type constructor.
  liftPFormatPrec ::
    (Int -> a -> Doc ann) -> ([a] -> Doc ann) -> Int -> f a -> Doc ann

  -- | Lift a pretty-printer to list of values with unary type constructors.
  liftPFormatList ::
    (Int -> a -> Doc ann) -> ([a] -> Doc ann) -> [f a] -> Doc ann
  liftPFormatList f l = align . prettyPrintList . map (liftPFormatPrec f l 0)

instance PPrint1 [] where
  liftPFormatPrec _ l _ = l
  liftPFormatList _ l = prettyPrintList . fmap l

-- | Lift the standard pretty-printer ('pformatPrec', 'pformatList') to unary
-- type constructors.
pformatPrec1 :: (PPrint1 f, PPrint a) => Int -> f a -> Doc ann
pformatPrec1 = liftPFormatPrec pformatPrec pformatList
{-# INLINE pformatPrec1 #-}

-- | Lift the standard pretty-printer ('pformatPrec', 'pformatList') to list of
-- values with unary type constructors.
pformatList1 :: (PPrint1 f, PPrint a) => [f a] -> Doc ann
pformatList1 = liftPFormatList pformatPrec pformatList
{-# INLINE pformatList1 #-}

-- | Lifting of the 'PPrint' class to binary type constructors.
class
  ( forall a. (PPrint a) => PPrint1 (f a),
    forall a b. (PPrint a, PPrint b) => PPrint (f a b)
  ) =>
  PPrint2 f
  where
  -- | Lift two pretty-printers to a binary type constructor.
  liftPFormatPrec2 ::
    (Int -> a -> Doc ann) ->
    ([a] -> Doc ann) ->
    (Int -> b -> Doc ann) ->
    ([b] -> Doc ann) ->
    Int ->
    f a b ->
    Doc ann

  -- | Lift two pretty-printers to list of values with binary type constructors.
  liftPFormatList2 ::
    (Int -> a -> Doc ann) ->
    ([a] -> Doc ann) ->
    (Int -> b -> Doc ann) ->
    ([b] -> Doc ann) ->
    [f a b] ->
    Doc ann
  liftPFormatList2 fa fb la lb =
    align . prettyPrintList . map (liftPFormatPrec2 fa fb la lb 0)

-- | Lift the standard pretty-printer ('pformatPrec', 'pformatList') to binary
-- type constructors.
pformatPrec2 :: (PPrint2 f, PPrint a, PPrint b) => Int -> f a b -> Doc ann
pformatPrec2 = liftPFormatPrec2 pformatPrec pformatList pformatPrec pformatList
{-# INLINE pformatPrec2 #-}

-- | Lift the standard pretty-printer ('pformatPrec', 'pformatList') to list of
-- values with binary type constructors.
pformatList2 :: (PPrint2 f, PPrint a, PPrint b) => [f a b] -> Doc ann
pformatList2 = liftPFormatList2 pformatPrec pformatList pformatPrec pformatList
{-# INLINE pformatList2 #-}

-- | The arguments to the generic 'PPrint' class.
data family PPrintArgs arity a ann :: Type

data instance PPrintArgs Arity0 _ _ = PPrintArgs0

data instance PPrintArgs Arity1 a ann
  = PPrintArgs1
      ((Int -> a -> Doc ann))
      (([a] -> Doc ann))

-- | Controls how to pretty-print a generic representation.
data PPrintType = Rec | Tup | Pref | Inf String Int
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
pformatWithConstructor :: Int -> Doc ann -> [Doc ann] -> Doc ann
pformatWithConstructor n c l =
  group $ condEnclose (n > 10) "(" ")" $ align $ nest 2 $ vsep (c : l)

-- | Pretty print a list of fields with a constructor without alignment.
pformatWithConstructorNoAlign :: Int -> Doc ann -> [Doc ann] -> Doc ann
pformatWithConstructorNoAlign n c l =
  group $ condEnclose (n > 10) "(" ")" $ nest 2 $ vsep (c : l)

-- | Pretty print a value using 'showsPrec'.
viaShowsPrec :: (Int -> a -> ShowS) -> Int -> a -> Doc ann
viaShowsPrec f n a = pretty (f n a "")

-- | Generic 'PPrint' class.
class GPPrint arity f where
  gpformatPrec :: PPrintArgs arity a ann -> PPrintType -> Int -> f a -> Doc ann
  gpformatList :: (HasCallStack) => PPrintArgs arity a ann -> [f a] -> Doc ann
  gpformatList = error "generic format (gpformatList): unnecessary case"
  gisNullary :: (HasCallStack) => PPrintArgs arity a ann -> f a -> Bool
  gisNullary = error "generic format (isNullary): unnecessary case"

instance GPPrint arity V1 where
  gpformatPrec _ _ _ x = case x of {}

instance GPPrint arity U1 where
  gpformatPrec _ _ _ U1 = ""
  gisNullary _ _ = True

instance (PPrint c) => GPPrint arity (K1 i c) where
  gpformatPrec _ _ n (K1 a) = pformatPrec n a
  gisNullary _ _ = False

instance (GPPrint arity a, Constructor c) => GPPrint arity (C1 c a) where
  gpformatPrec arg _ n c@(M1 x) =
    case t of
      Tup ->
        prettyBraces t (gpformatPrec arg t 0 x)
      Inf _ m ->
        group $ condEnclose (n > m) "(" ")" $ gpformatPrec arg t m x
      _ ->
        if gisNullary arg x
          then pformat (conName c)
          else
            pformatWithConstructorNoAlign
              n
              (pformat (conName c))
              [prettyBraces t (gpformatPrec arg t 11 x)]
    where
      prettyBraces :: PPrintType -> Doc ann -> Doc ann
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

instance (Selector s, GPPrint arity a) => GPPrint arity (S1 s a) where
  gpformatPrec arg t n s@(M1 x)
    | selName s == "" =
        case t of
          Pref -> gpformatPrec arg t (n + 1) x
          _ -> gpformatPrec arg t (n + 1) x
    | otherwise =
        group $
          align $
            nest 2 $
              vsep [pretty (selName s) <+> "=", gpformatPrec arg t 0 x]
  gisNullary _ _ = False

instance (GPPrint arity a) => GPPrint arity (D1 d a) where
  gpformatPrec arg _ n (M1 x) = gpformatPrec arg Pref n x
  gpformatList arg = align . prettyPrintList . fmap (gpformatPrec arg Pref 0)

instance (GPPrint arity a, GPPrint arity b) => GPPrint arity (a :+: b) where
  gpformatPrec arg t n (L1 x) = gpformatPrec arg t n x
  gpformatPrec arg t n (R1 x) = gpformatPrec arg t n x

instance (GPPrint arity a, GPPrint arity b) => GPPrint arity (a :*: b) where
  gpformatPrec arg t@Rec n (a :*: b) =
    align $
      vcat
        [ gpformatPrec arg t n a <> "," <> flatAlt "" " ",
          gpformatPrec arg t n b
        ]
  gpformatPrec arg t@(Inf s _) n (a :*: b) =
    nest 2 $
      vsep
        [ align $ gpformatPrec arg t n a,
          pretty s <+> gpformatPrec arg t n b
        ]
  gpformatPrec arg t@Tup _ (a :*: b) =
    vcat
      [ gpformatPrec arg t 0 a <> "," <> flatAlt "" " ",
        gpformatPrec arg t 0 b
      ]
  gpformatPrec arg t@Pref n (a :*: b) =
    vsep
      [ gpformatPrec arg t (n + 1) a,
        gpformatPrec arg t (n + 1) b
      ]
  gisNullary _ _ = False

instance GPPrint Arity1 Par1 where
  gpformatPrec (PPrintArgs1 f _) _ n (Par1 a) = f n a
  gpformatList (PPrintArgs1 _ g) l = g $ unPar1 <$> l

instance (PPrint1 f) => GPPrint Arity1 (Rec1 f) where
  gpformatPrec (PPrintArgs1 f g) _ n (Rec1 x) = liftPFormatPrec f g n x
  gpformatList (PPrintArgs1 f g) l = liftPFormatList f g $ unRec1 <$> l

instance
  (PPrint1 f, GPPrint Arity1 g) =>
  GPPrint Arity1 (f :.: g)
  where
  gpformatPrec arg t n (Comp1 x) =
    liftPFormatPrec (gpformatPrec arg t) (gpformatList arg) n x
  gpformatList arg l =
    liftPFormatList (gpformatPrec arg Pref) (gpformatList arg) $ unComp1 <$> l

-- | Generic 'pformatPrec' function.
genericPFormatPrec ::
  (Generic a, GPPrint Arity0 (Rep a)) =>
  Int ->
  a ->
  Doc ann
genericPFormatPrec n = gpformatPrec PPrintArgs0 Pref n . from
{-# INLINE genericPFormatPrec #-}

-- | Generic 'pformatList' function.
genericPFormatList ::
  (Generic a, GPPrint Arity0 (Rep a)) =>
  [a] ->
  Doc ann
genericPFormatList = gpformatList PPrintArgs0 . fmap from
{-# INLINE genericPFormatList #-}

-- | Generic 'liftPFormatPrec' function.
genericLiftPFormatPrec ::
  (Generic1 f, GPPrint Arity1 (Rep1 f)) =>
  (Int -> a -> Doc ann) ->
  ([a] -> Doc ann) ->
  Int ->
  f a ->
  Doc ann
genericLiftPFormatPrec p l n = gpformatPrec (PPrintArgs1 p l) Pref n . from1
{-# INLINE genericLiftPFormatPrec #-}

-- | Generic 'liftPFormatList' function.
genericLiftPFormatList ::
  (Generic1 f, GPPrint Arity1 (Rep1 f)) =>
  (Int -> a -> Doc ann) ->
  ([a] -> Doc ann) ->
  [f a] ->
  Doc ann
genericLiftPFormatList p l = gpformatList (PPrintArgs1 p l) . fmap from1
{-# INLINE genericLiftPFormatList #-}

instance
  (Generic a, GPPrint Arity0 (Rep a)) =>
  PPrint (Default a)
  where
  pformatPrec n = genericPFormatPrec n . unDefault
  pformatList = genericPFormatList . fmap unDefault

instance
  (Generic1 f, GPPrint Arity1 (Rep1 f), PPrint a) =>
  PPrint (Default1 f a)
  where
  pformatPrec = pformatPrec1
  pformatList = pformatList1

instance
  (Generic1 f, GPPrint Arity1 (Rep1 f)) =>
  PPrint1 (Default1 f)
  where
  liftPFormatPrec p l n = genericLiftPFormatPrec p l n . unDefault1
  liftPFormatList p l = genericLiftPFormatList p l . fmap unDefault1

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

-- Prettyprint
#define FORMAT_SYM_SIMPLE(symtype) \
instance PPrint symtype where \
  pformat (symtype t) = prettyPrintTerm t

#define FORMAT_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => PPrint (symtype n) where \
  pformat (symtype t) = prettyPrintTerm t

#define FORMAT_SYM_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb)\
  => PPrint (sa op sb) where \
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

-- Instance
deriveBuiltins
  (ViaDefault ''PPrint)
  [''PPrint]
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
    ''BitwidthMismatch,
    ''NotRepresentableFPError,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

deriveBuiltins
  (ViaDefault1 ''PPrint1)
  [''PPrint, ''PPrint1]
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
instance (PPrint a) => PPrint (Identity a) where
  pformatPrec = pformatPrec1

instance PPrint1 Identity where
  liftPFormatPrec f _ n (Identity a) = f n a

-- MaybeT
instance
  (PPrint1 m, PPrint a) =>
  PPrint (MaybeT m a)
  where
  pformatPrec = pformatPrec1

instance
  (PPrint1 m) =>
  PPrint1 (MaybeT m)
  where
  liftPFormatPrec f l n (MaybeT a) =
    pformatWithConstructor
      n
      "MaybeT"
      [liftPFormatPrec (liftPFormatPrec f l) (liftPFormatList f l) 11 a]

-- ExceptT
instance
  (PPrint1 m, PPrint e, PPrint a) =>
  PPrint (ExceptT e m a)
  where
  pformatPrec = pformatPrec1

instance
  (PPrint1 m, PPrint e) =>
  PPrint1 (ExceptT e m)
  where
  liftPFormatPrec f l n (ExceptT a) =
    pformatWithConstructor
      n
      "ExceptT"
      [liftPFormatPrec (liftPFormatPrec f l) (liftPFormatList f l) 11 a]

-- WriterT
instance
  (PPrint1 m, PPrint a, PPrint w) =>
  PPrint (WriterLazy.WriterT w m a)
  where
  pformatPrec = pformatPrec1

instance
  (PPrint1 m, PPrint w) =>
  PPrint1 (WriterLazy.WriterT w m)
  where
  liftPFormatPrec f l n (WriterLazy.WriterT a) =
    pformatWithConstructor
      n
      "WriterT"
      [ liftPFormatPrec
          (liftPFormatPrec2 f l pformatPrec pformatList)
          (liftPFormatList2 f l pformatPrec pformatList)
          11
          a
      ]

instance
  (PPrint1 m, PPrint a, PPrint w) =>
  PPrint (WriterStrict.WriterT w m a)
  where
  pformatPrec = pformatPrec1

instance
  (PPrint1 m, PPrint w) =>
  PPrint1 (WriterStrict.WriterT w m)
  where
  liftPFormatPrec f l n (WriterStrict.WriterT a) =
    pformatWithConstructor
      n
      "WriterT"
      [ liftPFormatPrec
          (liftPFormatPrec2 f l pformatPrec pformatList)
          (liftPFormatList2 f l pformatPrec pformatList)
          11
          a
      ]

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

-- PPrint2
instance PPrint2 Either where
  liftPFormatPrec2 fe _ _ _ n (Left e) =
    pformatWithConstructor n "Left" [fe 11 e]
  liftPFormatPrec2 _ _ fa _ n (Right a) =
    pformatWithConstructor n "Right" [fa 11 a]

instance PPrint2 (,) where
  liftPFormatPrec2 fa _ fb _ _ (a, b) =
    prettyPrintTuple [fa 0 a, fb 0 b]

instance (PPrint a) => PPrint2 ((,,) a) where
  liftPFormatPrec2 fb _ fc _ _ (a, b, c) =
    prettyPrintTuple [pformat a, fb 0 b, fc 0 c]

instance (PPrint a, PPrint b) => PPrint2 ((,,,) a b) where
  liftPFormatPrec2 fc _ fd _ _ (a, b, c, d) =
    prettyPrintTuple [pformat a, pformat b, fc 0 c, fd 0 d]

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
      pformatSymbolWithoutType (SomeTypedSymbol _ s) = pformat $ unTypedSymbol s
      pformatPair :: (SomeTypedSymbol knd, ModelValue) -> Doc ann
      pformatPair (s, v) = pformatSymbolWithoutType s <> " -> " <> pformat v
      bodyFormatted = pformatListLike "{" "}" $ pformatPair <$> HM.toList m

instance PPrint (SymbolSet knd) where
  pformatPrec n (SymbolSet s) =
    pformatWithConstructor
      n
      "SymbolSet"
      [pformatListLike "{" "}" $ pformat <$> HS.toList s]
