{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.SExpr
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.SExpr
  ( SExpr (..),
    showsSExprWithParens,
    parseFileLocation,
    fileLocation,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import Data.Serialize.Text ()
import qualified Data.Text as T
import Debug.Trace.LocationTH (__LOCATION__)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift, unsafeTExpCoerce)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, liftSplice)

-- | S-expression data type. Used for symbol metadata.
data SExpr = Atom T.Text | List [SExpr] | NumberAtom Integer | BoolAtom Bool
  deriving stock (Eq, Ord, Generic, Lift)
  deriving anyclass (Hashable, NFData, Serial)

instance Cereal.Serialize SExpr where
  put = serialize
  get = deserialize

instance Binary.Binary SExpr where
  put = serialize
  get = deserialize

instance Show SExpr where
  showsPrec _ = showsSExprWithParens '(' ')'

unwordsS :: [ShowS] -> ShowS
unwordsS [] = id
unwordsS [x] = x
unwordsS (x : xs) = x . showString " " . unwordsS xs

-- | Show an S-expression with specific parentheses.
showsSExprWithParens :: Char -> Char -> SExpr -> ShowS
showsSExprWithParens _ _ (Atom s) = showString $ T.unpack s
showsSExprWithParens lp rp (List l) =
  showString [lp] . unwordsS (map shows l) . (showString [rp])
showsSExprWithParens _ _ (NumberAtom n) = shows n
showsSExprWithParens _ _ (BoolAtom b) = showString $ if b then "#t" else "#f"

-- | Parse a file location string into an S-expression.
parseFileLocation :: String -> SExpr
parseFileLocation str =
  let r = reverse str
      (s2, r1) = break (== '-') r
      (s1, r2) = break (== ':') $ tail r1
      (l, p) = break (== ':') $ tail r2
   in List
        [ Atom "grisette-file-location",
          Atom $ T.pack $ reverse $ tail p,
          NumberAtom $ read $ reverse l,
          List
            [ NumberAtom $ read $ reverse s1,
              NumberAtom $ read $ reverse s2
            ]
        ]

-- | Get the file location of the splice.
fileLocation :: SpliceQ SExpr
fileLocation =
  [||parseFileLocation $$(liftSplice $ unsafeTExpCoerce __LOCATION__)||]
