{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.IR.SymPrim.Data.Prim.BitsTests (bitsTests) where

import Grisette.Core.Data.BV
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
import Test.Tasty
import Test.Tasty.HUnit

bitsTests :: TestTree
bitsTests =
  testGroup
    "BitsTests"
    [ testGroup
        "AndBits"
        [ testCase "On both concrete" $ do
            pevalAndBitsTerm
              (conTerm 3 :: Term (WordN 4))
              (conTerm 5)
              @=? conTerm 1,
          testCase "On zeroBits" $ do
            pevalAndBitsTerm
              (conTerm 0 :: Term (WordN 4))
              (ssymTerm "a")
              @=? conTerm 0
            pevalAndBitsTerm
              (ssymTerm "a")
              (conTerm 0 :: Term (WordN 4))
              @=? conTerm 0,
          testCase "On all one bits" $ do
            pevalAndBitsTerm
              (conTerm 15 :: Term (WordN 4))
              (ssymTerm "a")
              @=? ssymTerm "a"
            pevalAndBitsTerm
              (ssymTerm "a")
              (conTerm 15 :: Term (WordN 4))
              @=? ssymTerm "a",
          testCase "On symbolic" $ do
            pevalAndBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @=? andBitsTerm
                (ssymTerm "a" :: Term (WordN 4))
                (ssymTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "OrBits"
        [ testCase "On both concrete" $ do
            pevalOrBitsTerm
              (conTerm 3 :: Term (WordN 4))
              (conTerm 5)
              @=? conTerm 7,
          testCase "On zeroBits" $ do
            pevalOrBitsTerm
              (conTerm 0 :: Term (WordN 4))
              (ssymTerm "a")
              @=? ssymTerm "a"
            pevalOrBitsTerm
              (ssymTerm "a")
              (conTerm 0 :: Term (WordN 4))
              @=? ssymTerm "a",
          testCase "On all one bits" $ do
            pevalOrBitsTerm
              (conTerm 15 :: Term (WordN 4))
              (ssymTerm "a")
              @=? conTerm 15
            pevalOrBitsTerm
              (ssymTerm "a")
              (conTerm 15 :: Term (WordN 4))
              @=? conTerm 15,
          testCase "On symbolic" $ do
            pevalOrBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @=? orBitsTerm
                (ssymTerm "a" :: Term (WordN 4))
                (ssymTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "XorBits"
        [ testCase "On both concrete" $ do
            pevalXorBitsTerm
              (conTerm 3 :: Term (WordN 4))
              (conTerm 5)
              @=? conTerm 6,
          testCase "On zeroBits" $ do
            pevalXorBitsTerm
              (conTerm 0 :: Term (WordN 4))
              (ssymTerm "a")
              @=? ssymTerm "a"
            pevalXorBitsTerm
              (ssymTerm "a")
              (conTerm 0 :: Term (WordN 4))
              @=? ssymTerm "a",
          testCase "On all one bits" $ do
            pevalXorBitsTerm
              (conTerm 15 :: Term (WordN 4))
              (ssymTerm "a")
              @=? pevalComplementBitsTerm (ssymTerm "a")
            pevalXorBitsTerm
              (ssymTerm "a")
              (conTerm 15 :: Term (WordN 4))
              @=? pevalComplementBitsTerm (ssymTerm "a"),
          testCase "On single complement" $ do
            pevalXorBitsTerm
              (pevalComplementBitsTerm $ ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @=? pevalComplementBitsTerm (pevalXorBitsTerm (ssymTerm "a") (ssymTerm "b"))
            pevalXorBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (pevalComplementBitsTerm $ ssymTerm "b")
              @=? pevalComplementBitsTerm (pevalXorBitsTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On both complement" $ do
            pevalXorBitsTerm
              (pevalComplementBitsTerm $ ssymTerm "a" :: Term (WordN 4))
              (pevalComplementBitsTerm $ ssymTerm "b")
              @=? pevalXorBitsTerm (ssymTerm "a") (ssymTerm "b"),
          testCase "On symbolic" $ do
            pevalXorBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @=? xorBitsTerm
                (ssymTerm "a" :: Term (WordN 4))
                (ssymTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "ComplementBits"
        [ testCase "On concrete" $ do
            pevalComplementBitsTerm (conTerm 5 :: Term (WordN 4)) @=? conTerm 10,
          testCase "On complement" $ do
            pevalComplementBitsTerm (pevalComplementBitsTerm (ssymTerm "a") :: Term (WordN 4)) @=? ssymTerm "a",
          testCase "On symbolic" $ do
            pevalComplementBitsTerm (ssymTerm "a" :: Term (WordN 4))
              @=? complementBitsTerm (ssymTerm "a" :: Term (WordN 4))
        ],
      testGroup
        "ShiftBits"
        [ testCase "On concrete" $ do
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) (-5) @=? conTerm 0
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) (-4) @=? conTerm 0
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) (-3) @=? conTerm 1
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) (-2) @=? conTerm 3
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) (-1) @=? conTerm 7
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) 0 @=? conTerm 15
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) 1 @=? conTerm 14
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) 2 @=? conTerm 12
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) 3 @=? conTerm 8
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) 4 @=? conTerm 0
            pevalShiftBitsTerm (conTerm 15 :: Term (WordN 4)) 5 @=? conTerm 0

            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) (-5) @=? conTerm 15
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) (-4) @=? conTerm 15
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) (-3) @=? conTerm 15
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) (-2) @=? conTerm 15
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) (-1) @=? conTerm 15
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) 0 @=? conTerm 15
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) 1 @=? conTerm 14
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) 2 @=? conTerm 12
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) 3 @=? conTerm 8
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) 4 @=? conTerm 0
            pevalShiftBitsTerm (conTerm 15 :: Term (IntN 4)) 5 @=? conTerm 0,
          testCase "shift 0" $ do
            pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) 0 @=? ssymTerm "a"
            pevalShiftBitsTerm (ssymTerm "a" :: Term (IntN 4)) 0 @=? ssymTerm "a",
          testCase "shift left bitsize" $ do
            pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) 4 @=? conTerm 0
            pevalShiftBitsTerm (ssymTerm "a" :: Term (IntN 4)) 4 @=? conTerm 0
            pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) 5 @=? conTerm 0
            pevalShiftBitsTerm (ssymTerm "a" :: Term (IntN 4)) 5 @=? conTerm 0,
          testCase "shift same direction twice" $ do
            pevalShiftBitsTerm (pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) 1) 2
              @=? pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) 3
            pevalShiftBitsTerm (pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) (-1)) (-2)
              @=? pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) (-3),
          testCase "shift symbolic" $ do
            pevalShiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) 2
              @=? shiftBitsTerm (ssymTerm "a" :: Term (WordN 4)) 2
        ],
      testGroup
        "Rotate"
        [ testCase "On concrete" $ do
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) (-4) @=? conTerm 3
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) (-3) @=? conTerm 6
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) (-2) @=? conTerm 12
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) (-1) @=? conTerm 9
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) 0 @=? conTerm 3
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) 1 @=? conTerm 6
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) 2 @=? conTerm 12
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) 3 @=? conTerm 9
            pevalRotateBitsTerm (conTerm 3 :: Term (WordN 4)) 4 @=? conTerm 3,
          testCase "rotate 0" $ do
            pevalRotateBitsTerm (ssymTerm "a" :: Term (WordN 4)) 0 @=? ssymTerm "a",
          testCase "rotate extra bits" $ do
            pevalRotateBitsTerm (ssymTerm "a" :: Term (WordN 4)) 4 @=? ssymTerm "a"
            pevalRotateBitsTerm (ssymTerm "a" :: Term (WordN 4)) 5
              @=? pevalRotateBitsTerm (ssymTerm "a") 1
            pevalRotateBitsTerm (ssymTerm "a" :: Term (WordN 4)) (-1)
              @=? pevalRotateBitsTerm (ssymTerm "a") 3,
          testCase "rotate twice" $ do
            pevalRotateBitsTerm (pevalRotateBitsTerm (ssymTerm "a" :: Term (WordN 4)) 1) 2
              @=? pevalRotateBitsTerm (ssymTerm "a") 3,
          testCase "rotate symbolic" $ do
            pevalRotateBitsTerm (ssymTerm "a" :: Term (WordN 4)) 2
              @=? rotateBitsTerm (ssymTerm "a" :: Term (WordN 4)) 2
        ]
    ]
