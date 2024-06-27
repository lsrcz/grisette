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

module Grisette.Internal.Core.Data.Class.GPretty
  ( -- * Pretty printing
    GPretty (..),
    docToTextWith,
    docToTextWithWidth,
    docToText,
    formatTextWith,
    formatTextWithWidth,
    formatText,
    GPretty1 (..),
    gprettyPrec1,
    gprettyList1,
    GPretty2 (..),
    gprettyPrec2,
    gprettyList2,

    -- * Generic 'GPretty'
    genericGPrettyPrec,
    genericLiftGPrettyPrec,
    GPrettyArgs (..),
    GGPretty (..),
    PrettyType (..),

    -- * Helpers
    groupedEnclose,
    condEnclose,
    prettyWithConstructor,
    prettyWithConstructorNoAlign,
    viaShowsPrec,

    -- * Re-exports
    module Prettyprinter,
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
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
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

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
#else
import Data.Text.Prettyprint.Doc as Prettyprinter
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
#endif

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
class GPretty a where
  gpretty :: a -> Doc ann
  gprettyPrec :: Int -> a -> Doc ann
  gprettyList :: [a] -> Doc ann
  gprettyList = align . prettyPrintList . map gpretty

  gpretty = gprettyPrec 0
  gprettyPrec _ = gpretty

  {-# MINIMAL gpretty | gprettyPrec #-}

instance GPretty Char where
  gpretty = viaShow
  gprettyList v = pretty (fromString v :: T.Text)

instance (GPretty a) => GPretty [a] where
  gpretty = gprettyList

instance GPretty1 [] where
  liftGPrettyPrec _ l _ = l
  liftGPrettyList _ l = prettyPrintList . fmap l

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
formatTextWith :: (GPretty a) => LayoutOptions -> a -> T.Text
formatTextWith options = docToTextWith options . gpretty

-- | Convenience function to format a value to 'T.Text'.
--
-- You can control the layout with a single number of the width limit.
formatTextWithWidth :: (GPretty a) => Int -> a -> T.Text
formatTextWithWidth n = docToTextWithWidth n . gpretty

-- | Convenience function to format a value to 'T.Text'.
--
-- The default layout options 'defaultLayoutOptions' are used.
formatText :: (GPretty a) => a -> T.Text
formatText = docToText . gpretty

-- | Lifting of the 'GPretty' class to unary type constructors.
class (forall a. (GPretty a) => GPretty (f a)) => GPretty1 f where
  -- | Lift a pretty-printer to a unary type constructor.
  liftGPrettyPrec ::
    (Int -> a -> Doc ann) -> ([a] -> Doc ann) -> Int -> f a -> Doc ann

  -- | Lift a pretty-printer to list of values with unary type constructors.
  liftGPrettyList ::
    (Int -> a -> Doc ann) -> ([a] -> Doc ann) -> [f a] -> Doc ann
  liftGPrettyList f l = align . prettyPrintList . map (liftGPrettyPrec f l 0)

-- | Lift the standard pretty-printer ('gprettyPrec', 'gprettyList') to unary
-- type constructors.
gprettyPrec1 :: (GPretty1 f, GPretty a) => Int -> f a -> Doc ann
gprettyPrec1 = liftGPrettyPrec gprettyPrec gprettyList
{-# INLINE gprettyPrec1 #-}

-- | Lift the standard pretty-printer ('gprettyPrec', 'gprettyList') to list of
-- values with unary type constructors.
gprettyList1 :: (GPretty1 f, GPretty a) => [f a] -> Doc ann
gprettyList1 = liftGPrettyList gprettyPrec gprettyList
{-# INLINE gprettyList1 #-}

-- | Lifting of the 'GPretty' class to binary type constructors.
class
  ( forall a. (GPretty a) => GPretty1 (f a),
    forall a b. (GPretty a, GPretty b) => GPretty (f a b)
  ) =>
  GPretty2 f
  where
  -- | Lift two pretty-printers to a binary type constructor.
  liftGPrettyPrec2 ::
    (Int -> a -> Doc ann) ->
    (Int -> b -> Doc ann) ->
    ([a] -> Doc ann) ->
    ([b] -> Doc ann) ->
    Int ->
    f a b ->
    Doc ann

  -- | Lift two pretty-printers to list of values with binary type constructors.
  liftGPrettyList2 ::
    (Int -> a -> Doc ann) ->
    (Int -> b -> Doc ann) ->
    ([a] -> Doc ann) ->
    ([b] -> Doc ann) ->
    [f a b] ->
    Doc ann
  liftGPrettyList2 fa fb la lb =
    align . prettyPrintList . map (liftGPrettyPrec2 fa fb la lb 0)

-- | Lift the standard pretty-printer ('gprettyPrec', 'gprettyList') to binary
-- type constructors.
gprettyPrec2 :: (GPretty2 f, GPretty a, GPretty b) => Int -> f a b -> Doc ann
gprettyPrec2 = liftGPrettyPrec2 gprettyPrec gprettyPrec gprettyList gprettyList
{-# INLINE gprettyPrec2 #-}

-- | Lift the standard pretty-printer ('gprettyPrec', 'gprettyList') to list of
-- values with binary type constructors.
gprettyList2 :: (GPretty2 f, GPretty a, GPretty b) => [f a b] -> Doc ann
gprettyList2 = liftGPrettyList2 gprettyPrec gprettyPrec gprettyList gprettyList
{-# INLINE gprettyList2 #-}

-- | The arguments to the generic 'GPretty' class.
data family GPrettyArgs arity a ann :: Type

data instance GPrettyArgs Arity0 _ _ = GPrettyArgs0

data instance GPrettyArgs Arity1 a ann
  = GPrettyArgs1
      ((Int -> a -> Doc ann))
      (([a] -> Doc ann))

-- | Controls how to pretty-print a generic representation.
data PrettyType = Rec | Tup | Pref | Inf String Int
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
prettyWithConstructor :: Int -> Doc ann -> [Doc ann] -> Doc ann
prettyWithConstructor n c l =
  group $ condEnclose (n > 10) "(" ")" $ align $ nest 2 $ vsep (c : l)

-- | Pretty print a list of fields with a constructor without alignment.
prettyWithConstructorNoAlign :: Int -> Doc ann -> [Doc ann] -> Doc ann
prettyWithConstructorNoAlign n c l =
  group $ condEnclose (n > 10) "(" ")" $ nest 2 $ vsep (c : l)

-- | Pretty print a value using 'showsPrec'.
viaShowsPrec :: (Int -> a -> ShowS) -> Int -> a -> Doc ann
viaShowsPrec f n a = pretty (f n a "")

-- | Generic 'GPretty' class.
class GGPretty arity f where
  ggprettyPrec :: GPrettyArgs arity a ann -> PrettyType -> Int -> f a -> Doc ann
  ggprettyList :: (HasCallStack) => GPrettyArgs arity a ann -> [f a] -> Doc ann
  ggprettyList = error "generic gpretty (ggprettyList): unnecessary case"
  gisNullary :: (HasCallStack) => GPrettyArgs arity a ann -> f a -> Bool
  gisNullary = error "generic gpretty (isNullary): unnecessary case"

instance GGPretty arity V1 where
  ggprettyPrec _ _ _ x = case x of {}

instance GGPretty arity U1 where
  ggprettyPrec _ _ _ U1 = ""
  gisNullary _ _ = True

instance (GPretty c) => GGPretty arity (K1 i c) where
  ggprettyPrec _ _ n (K1 a) = gprettyPrec n a
  gisNullary _ _ = False

instance (GGPretty arity a, Constructor c) => GGPretty arity (C1 c a) where
  ggprettyPrec arg _ n c@(M1 x) =
    case t of
      Tup ->
        prettyBraces t (ggprettyPrec arg t 0 x)
      Inf _ m ->
        group $ condEnclose (n > m) "(" ")" $ ggprettyPrec arg t m x
      _ ->
        if gisNullary arg x
          then gpretty (conName c)
          else
            prettyWithConstructorNoAlign
              n
              (gpretty (conName c))
              [prettyBraces t (ggprettyPrec arg t 11 x)]
    where
      prettyBraces :: PrettyType -> Doc ann -> Doc ann
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

instance (Selector s, GGPretty arity a) => GGPretty arity (S1 s a) where
  ggprettyPrec arg t n s@(M1 x)
    | selName s == "" =
        case t of
          Pref -> ggprettyPrec arg t (n + 1) x
          _ -> ggprettyPrec arg t (n + 1) x
    | otherwise =
        group $
          align $
            nest 2 $
              vsep [pretty (selName s) <+> "=", ggprettyPrec arg t 0 x]
  gisNullary _ _ = False

instance (GGPretty arity a) => GGPretty arity (D1 d a) where
  ggprettyPrec arg _ n (M1 x) = ggprettyPrec arg Pref n x
  ggprettyList arg = align . prettyPrintList . fmap (ggprettyPrec arg Pref 0)

instance (GGPretty arity a, GGPretty arity b) => GGPretty arity (a :+: b) where
  ggprettyPrec arg t n (L1 x) = ggprettyPrec arg t n x
  ggprettyPrec arg t n (R1 x) = ggprettyPrec arg t n x

instance (GGPretty arity a, GGPretty arity b) => GGPretty arity (a :*: b) where
  ggprettyPrec arg t@Rec n (a :*: b) =
    align $
      vcat
        [ ggprettyPrec arg t n a <> "," <> flatAlt "" " ",
          ggprettyPrec arg t n b
        ]
  ggprettyPrec arg t@(Inf s _) n (a :*: b) =
    nest 2 $
      vsep
        [ align $ ggprettyPrec arg t n a,
          pretty s <+> ggprettyPrec arg t n b
        ]
  ggprettyPrec arg t@Tup _ (a :*: b) =
    vcat
      [ ggprettyPrec arg t 0 a <> "," <> flatAlt "" " ",
        ggprettyPrec arg t 0 b
      ]
  ggprettyPrec arg t@Pref n (a :*: b) =
    vsep
      [ ggprettyPrec arg t (n + 1) a,
        ggprettyPrec arg t (n + 1) b
      ]
  gisNullary _ _ = False

instance GGPretty Arity1 Par1 where
  ggprettyPrec (GPrettyArgs1 f _) _ n (Par1 a) = f n a
  ggprettyList (GPrettyArgs1 _ g) l = g $ unPar1 <$> l

instance (GPretty1 f) => GGPretty Arity1 (Rec1 f) where
  ggprettyPrec (GPrettyArgs1 f g) _ n (Rec1 x) = liftGPrettyPrec f g n x
  ggprettyList (GPrettyArgs1 f g) l = liftGPrettyList f g $ unRec1 <$> l

instance
  (GPretty1 f, GGPretty Arity1 g) =>
  GGPretty Arity1 (f :.: g)
  where
  ggprettyPrec arg t n (Comp1 x) =
    liftGPrettyPrec (ggprettyPrec arg t) (ggprettyList arg) n x
  ggprettyList arg l =
    liftGPrettyList (ggprettyPrec arg Pref) (ggprettyList arg) $ unComp1 <$> l

genericGPrettyPrec ::
  (Generic a, GGPretty Arity0 (Rep a)) =>
  Int ->
  a ->
  Doc ann
genericGPrettyPrec n = ggprettyPrec GPrettyArgs0 Pref n . from
{-# INLINE genericGPrettyPrec #-}

genericGPrettyList ::
  (Generic a, GGPretty Arity0 (Rep a)) =>
  [a] ->
  Doc ann
genericGPrettyList = ggprettyList GPrettyArgs0 . fmap from
{-# INLINE genericGPrettyList #-}

genericLiftGPrettyPrec ::
  (Generic1 f, GGPretty Arity1 (Rep1 f)) =>
  (Int -> a -> Doc ann) ->
  ([a] -> Doc ann) ->
  Int ->
  f a ->
  Doc ann
genericLiftGPrettyPrec p l n = ggprettyPrec (GPrettyArgs1 p l) Pref n . from1
{-# INLINE genericLiftGPrettyPrec #-}

genericLiftGPrettyList ::
  (Generic1 f, GGPretty Arity1 (Rep1 f)) =>
  (Int -> a -> Doc ann) ->
  ([a] -> Doc ann) ->
  [f a] ->
  Doc ann
genericLiftGPrettyList p l = ggprettyList (GPrettyArgs1 p l) . fmap from1
{-# INLINE genericLiftGPrettyList #-}

instance
  (Generic a, GGPretty Arity0 (Rep a)) =>
  GPretty (Default a)
  where
  gprettyPrec n = genericGPrettyPrec n . unDefault
  gprettyList = genericGPrettyList . fmap unDefault

instance
  (Generic1 f, GGPretty Arity1 (Rep1 f), GPretty a) =>
  GPretty (Default1 f a)
  where
  gprettyPrec = gprettyPrec1
  gprettyList = gprettyList1

instance
  (Generic1 f, GGPretty Arity1 (Rep1 f)) =>
  GPretty1 (Default1 f)
  where
  liftGPrettyPrec p l n = genericLiftGPrettyPrec p l n . unDefault1
  liftGPrettyList p l = genericLiftGPrettyList p l . fmap unDefault1

-- Instance
deriveBuiltins
  (ViaDefault ''GPretty)
  [''GPretty]
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
    ''VerificationConditions
  ]

deriveBuiltins
  (ViaDefault1 ''GPretty1)
  [''GPretty, ''GPretty1]
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
    ''(,,,,,,,,,,,,,,)
  ]

-- Identity
instance (GPretty a) => GPretty (Identity a) where
  gprettyPrec = gprettyPrec1

instance GPretty1 Identity where
  liftGPrettyPrec f _ n (Identity a) =
    prettyWithConstructor n "Identity" [f 11 a]

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (GPretty (f a), GPretty (g a)) =>
    GPretty (Sum f g a)

deriving via
  (Default1 (Sum f g))
  instance
    (GPretty1 f, GPretty1 g) =>
    GPretty1 (Sum f g)

-- MaybeT
instance
  (GPretty1 m, GPretty a) =>
  GPretty (MaybeT m a)
  where
  gprettyPrec = gprettyPrec1

instance
  (GPretty1 m) =>
  GPretty1 (MaybeT m)
  where
  liftGPrettyPrec f l n (MaybeT a) =
    prettyWithConstructor
      n
      "MaybeT"
      [liftGPrettyPrec (liftGPrettyPrec f l) (liftGPrettyList f l) 11 a]

-- ExceptT
instance
  (GPretty1 m, GPretty e, GPretty a) =>
  GPretty (ExceptT e m a)
  where
  gprettyPrec = gprettyPrec1

instance
  (GPretty1 m, GPretty e) =>
  GPretty1 (ExceptT e m)
  where
  liftGPrettyPrec f l n (ExceptT a) =
    prettyWithConstructor
      n
      "ExceptT"
      [liftGPrettyPrec (liftGPrettyPrec f l) (liftGPrettyList f l) 11 a]

-- WriterT
instance
  (GPretty1 m, GPretty a, GPretty w) =>
  GPretty (WriterLazy.WriterT w m a)
  where
  gprettyPrec = gprettyPrec1

instance
  (GPretty1 m, GPretty w) =>
  GPretty1 (WriterLazy.WriterT w m)
  where
  liftGPrettyPrec f l n (WriterLazy.WriterT a) =
    prettyWithConstructor
      n
      "WriterT"
      [ liftGPrettyPrec
          (liftGPrettyPrec2 f gprettyPrec l gprettyList)
          (liftGPrettyList2 f gprettyPrec l gprettyList)
          11
          a
      ]

instance
  (GPretty1 m, GPretty a, GPretty w) =>
  GPretty (WriterStrict.WriterT w m a)
  where
  gprettyPrec = gprettyPrec1

instance
  (GPretty1 m, GPretty w) =>
  GPretty1 (WriterStrict.WriterT w m)
  where
  liftGPrettyPrec f l n (WriterStrict.WriterT a) =
    prettyWithConstructor
      n
      "WriterT"
      [ liftGPrettyPrec
          (liftGPrettyPrec2 f gprettyPrec l gprettyList)
          (liftGPrettyList2 f gprettyPrec l gprettyList)
          11
          a
      ]

-- IdentityT
instance (GPretty1 m, GPretty a) => GPretty (IdentityT m a) where
  gprettyPrec = gprettyPrec1

instance (GPretty1 m) => GPretty1 (IdentityT m) where
  liftGPrettyPrec f l n (IdentityT a) =
    prettyWithConstructor n "IdentityT" [liftGPrettyPrec f l 11 a]

-- GPretty2
instance GPretty2 Either where
  liftGPrettyPrec2 fe _ _ _ n (Left e) =
    prettyWithConstructor n "Left" [fe 11 e]
  liftGPrettyPrec2 _ fa _ _ n (Right a) =
    prettyWithConstructor n "Right" [fa 11 a]

instance GPretty2 (,) where
  liftGPrettyPrec2 fa fb _ _ _ (a, b) =
    prettyPrintTuple [fa 0 a, fb 0 b]

instance (GPretty a) => GPretty2 ((,,) a) where
  liftGPrettyPrec2 fb fc _ _ _ (a, b, c) =
    prettyPrintTuple [gpretty a, fb 0 b, fc 0 c]

instance (GPretty a, GPretty b) => GPretty2 ((,,,) a b) where
  liftGPrettyPrec2 fc fd _ _ _ (a, b, c, d) =
    prettyPrintTuple [gpretty a, gpretty b, fc 0 c, fd 0 d]

#define GPRETTY_SIMPLE(type) \
instance GPretty type where gprettyPrec = viaShowsPrec showsPrec

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
GPRETTY_SIMPLE(Float)
GPRETTY_SIMPLE(Double)
GPRETTY_SIMPLE(FPRoundingMode)
#endif

instance GPretty B.ByteString where
  gpretty = pretty . C.unpack

instance GPretty T.Text where
  gpretty = pretty

instance (KnownNat n, 1 <= n) => GPretty (IntN n) where
  gpretty = viaShow

instance (KnownNat n, 1 <= n) => GPretty (WordN n) where
  gpretty = viaShow

instance (ValidFP eb sb) => GPretty (FP eb sb) where
  gpretty = viaShow

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
GPRETTY_SYM_SIMPLE(SymFPRoundingMode)
GPRETTY_SYM_BV(SymIntN)
GPRETTY_SYM_BV(SymWordN)
GPRETTY_SYM_FUN(=~>, SymTabularFun)
GPRETTY_SYM_FUN(-~>, SymGeneralFun)
#endif

instance (ValidFP eb sb) => GPretty (SymFP eb sb) where
  gpretty (SymFP t) = prettyPrintTerm t

instance (GPretty a) => GPretty (HS.HashSet a) where
  gprettyPrec n s =
    prettyWithConstructor n "fromList" [gprettyPrec 11 $ HS.toList s]

instance (GPretty k, GPretty v) => GPretty (HM.HashMap k v) where
  gprettyPrec n s =
    prettyWithConstructor n "fromList" [gprettyPrec 11 $ HM.toList s]
