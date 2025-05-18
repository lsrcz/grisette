{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.UnionBase
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.UnionBase
  (
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (align, group, nest, vsep)
#else
import Data.Text.Prettyprint.Doc (align, group, nest, vsep)
#endif

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), rnf1)
import qualified Data.Binary as Binary
import Data.Bytes.Get (MonadGet (getWord8))
import Data.Bytes.Put (MonadPut (putWord8))
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Show1 (liftShowsPrec),
    showsPrec1,
    showsUnaryWith,
  )
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Serialize as Cereal
import Grisette.Internal.Core.Data.Class.AsKey
  ( KeyHashable (keyHashWithSalt),
    KeyHashable1 (liftKeyHashWithSalt),
  )
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
  )
import Grisette.Internal.Core.Data.Class.PPrint
  ( PPrint (pformatPrec),
    PPrint1 (liftPFormatPrec),
    condEnclose,
    pformatPrec1,
  )
import Grisette.Internal.Internal.Decl.Core.Data.UnionBase
  ( UnionBase (UnionIf, UnionSingle),
    ifWithStrategy,
  )
import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    AllSyms1 (liftAllSymsS),
    SomeSym (SomeSym),
  )

instance Eq1 UnionBase where
  liftEq e (UnionSingle a) (UnionSingle b) = e a b
  liftEq e (UnionIf l1 i1 c1 t1 f1) (UnionIf l2 i2 c2 t2 f2) =
    e l1 l2 && i1 == i2 && c1 == c2 && liftEq e t1 t2 && liftEq e f1 f2
  liftEq _ _ _ = False

instance (NFData a) => NFData (UnionBase a) where
  rnf = rnf1

instance NFData1 UnionBase where
  liftRnf _a (UnionSingle a) = _a a
  liftRnf _a (UnionIf a bo b l r) =
    _a a `seq`
      rnf bo `seq`
        rnf b `seq`
          liftRnf _a l `seq`
            liftRnf _a r

instance (Mergeable a, Serial a) => Serial (UnionBase a) where
  serialize (UnionSingle a) = putWord8 0 >> serialize a
  serialize (UnionIf _ _ c a b) =
    putWord8 1 >> serialize c >> serialize a >> serialize b
  deserialize = do
    tag <- getWord8
    case tag of
      0 -> UnionSingle <$> deserialize
      1 ->
        ifWithStrategy rootStrategy
          <$> deserialize
          <*> deserialize
          <*> deserialize
      _ -> fail "Invalid tag"

instance (Mergeable a, Serial a) => Cereal.Serialize (UnionBase a) where
  put = serialize
  get = deserialize

instance (Mergeable a, Serial a) => Binary.Binary (UnionBase a) where
  put = serialize
  get = deserialize

instance Show1 UnionBase where
  liftShowsPrec sp _ i (UnionSingle a) = showsUnaryWith sp "Single" i a
  liftShowsPrec sp sl i (UnionIf _ _ cond t f) =
    showParen (i > 10) $
      showString "If"
        . showChar ' '
        . showsPrec 11 cond
        . showChar ' '
        . sp1 11 t
        . showChar ' '
        . sp1 11 f
    where
      sp1 = liftShowsPrec sp sl

instance (Show a) => Show (UnionBase a) where
  showsPrec = showsPrec1

instance (PPrint a) => PPrint (UnionBase a) where
  pformatPrec = pformatPrec1

instance PPrint1 UnionBase where
  liftPFormatPrec fa _ n (UnionSingle a) = fa n a
  liftPFormatPrec fa fl n (UnionIf _ _ cond t f) =
    group $
      condEnclose (n > 10) "(" ")" $
        align $
          nest 2 $
            vsep
              [ "If",
                pformatPrec 11 cond,
                liftPFormatPrec fa fl 11 t,
                liftPFormatPrec fa fl 11 f
              ]

instance (Hashable a) => KeyHashable (UnionBase a) where
  keyHashWithSalt = liftKeyHashWithSalt hashWithSalt
  {-# INLINE keyHashWithSalt #-}

instance KeyHashable1 UnionBase where
  liftKeyHashWithSalt f s (UnionSingle a) = s `hashWithSalt` (0 :: Int) `f` a
  liftKeyHashWithSalt f s (UnionIf _ _ c l r) =
    let g = liftKeyHashWithSalt f
     in ( s
            `hashWithSalt` (1 :: Int)
              `keyHashWithSalt` c
        )
          `g` l
          `g` r

instance (AllSyms a) => AllSyms (UnionBase a) where
  allSymsS (UnionSingle v) = allSymsS v
  allSymsS (UnionIf _ _ c t f) = \l -> SomeSym c : (allSymsS t . allSymsS f $ l)

instance AllSyms1 UnionBase where
  liftAllSymsS fa (UnionSingle v) = fa v
  liftAllSymsS fa (UnionIf _ _ c t f) =
    \l -> SomeSym c : (liftAllSymsS fa t . liftAllSymsS fa f $ l)
