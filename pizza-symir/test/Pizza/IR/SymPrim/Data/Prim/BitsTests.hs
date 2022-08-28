{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.IR.SymPrim.Data.Prim.BitsTests where

import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bits
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
              (concTerm 3 :: Term (WordN 4))
              (concTerm 5)
              @=? concTerm 1,
          testCase "On zeroBits" $ do
            pevalAndBitsTerm
              (concTerm 0 :: Term (WordN 4))
              (ssymbTerm "a")
              @=? concTerm 0
            pevalAndBitsTerm
              (ssymbTerm "a")
              (concTerm 0 :: Term (WordN 4))
              @=? concTerm 0,
          testCase "On all one bits" $ do
            pevalAndBitsTerm
              (concTerm 15 :: Term (WordN 4))
              (ssymbTerm "a")
              @=? ssymbTerm "a"
            pevalAndBitsTerm
              (ssymbTerm "a")
              (concTerm 15 :: Term (WordN 4))
              @=? ssymbTerm "a",
          testCase "On symbolic" $ do
            pevalAndBitsTerm
              (ssymbTerm "a" :: Term (WordN 4))
              (ssymbTerm "b")
              @=? andBitsTerm
                (ssymbTerm "a" :: Term (WordN 4))
                (ssymbTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "OrBits"
        [ testCase "On both concrete" $ do
            pevalOrBitsTerm
              (concTerm 3 :: Term (WordN 4))
              (concTerm 5)
              @=? concTerm 7,
          testCase "On zeroBits" $ do
            pevalOrBitsTerm
              (concTerm 0 :: Term (WordN 4))
              (ssymbTerm "a")
              @=? ssymbTerm "a"
            pevalOrBitsTerm
              (ssymbTerm "a")
              (concTerm 0 :: Term (WordN 4))
              @=? ssymbTerm "a",
          testCase "On all one bits" $ do
            pevalOrBitsTerm
              (concTerm 15 :: Term (WordN 4))
              (ssymbTerm "a")
              @=? concTerm 15
            pevalOrBitsTerm
              (ssymbTerm "a")
              (concTerm 15 :: Term (WordN 4))
              @=? concTerm 15,
          testCase "On symbolic" $ do
            pevalOrBitsTerm
              (ssymbTerm "a" :: Term (WordN 4))
              (ssymbTerm "b")
              @=? orBitsTerm
                (ssymbTerm "a" :: Term (WordN 4))
                (ssymbTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "XorBits"
        [ testCase "On both concrete" $ do
            pevalXorBitsTerm
              (concTerm 3 :: Term (WordN 4))
              (concTerm 5)
              @=? concTerm 6,
          testCase "On zeroBits" $ do
            pevalXorBitsTerm
              (concTerm 0 :: Term (WordN 4))
              (ssymbTerm "a")
              @=? ssymbTerm "a"
            pevalXorBitsTerm
              (ssymbTerm "a")
              (concTerm 0 :: Term (WordN 4))
              @=? ssymbTerm "a",
          testCase "On all one bits" $ do
            pevalXorBitsTerm
              (concTerm 15 :: Term (WordN 4))
              (ssymbTerm "a")
              @=? pevalComplementBitsTerm (ssymbTerm "a")
            pevalXorBitsTerm
              (ssymbTerm "a")
              (concTerm 15 :: Term (WordN 4))
              @=? pevalComplementBitsTerm (ssymbTerm "a"),
          testCase "On single complement" $ do
            pevalXorBitsTerm
              (pevalComplementBitsTerm $ ssymbTerm "a" :: Term (WordN 4))
              (ssymbTerm "b")
              @=? pevalComplementBitsTerm (pevalXorBitsTerm (ssymbTerm "a") (ssymbTerm "b"))
            pevalXorBitsTerm
              (ssymbTerm "a" :: Term (WordN 4))
              (pevalComplementBitsTerm $ ssymbTerm "b")
              @=? pevalComplementBitsTerm (pevalXorBitsTerm (ssymbTerm "a") (ssymbTerm "b")),
          testCase "On both complement" $ do
            pevalXorBitsTerm
              (pevalComplementBitsTerm $ ssymbTerm "a" :: Term (WordN 4))
              (pevalComplementBitsTerm $ ssymbTerm "b")
              @=? pevalXorBitsTerm (ssymbTerm "a") (ssymbTerm "b"),
          testCase "On symbolic" $ do
            pevalXorBitsTerm
              (ssymbTerm "a" :: Term (WordN 4))
              (ssymbTerm "b")
              @=? xorBitsTerm
                (ssymbTerm "a" :: Term (WordN 4))
                (ssymbTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "ComplementBits"
        [ testCase "On concrete" $ do
            pevalComplementBitsTerm (concTerm 5 :: Term (WordN 4)) @=? concTerm 10,
          testCase "On complement" $ do
            pevalComplementBitsTerm (pevalComplementBitsTerm (ssymbTerm "a") :: Term (WordN 4)) @=? ssymbTerm "a",
          testCase "On symbolic" $ do
            pevalComplementBitsTerm (ssymbTerm "a" :: Term (WordN 4))
              @=? complementBitsTerm (ssymbTerm "a" :: Term (WordN 4))
        ],
      testGroup
        "ShiftBits"
        [ testCase "On concrete" $ do
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-5) @=? concTerm 0
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-4) @=? concTerm 0
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-3) @=? concTerm 1
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-2) @=? concTerm 3
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) (-1) @=? concTerm 7
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 0 @=? concTerm 15
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 1 @=? concTerm 14
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 2 @=? concTerm 12
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 3 @=? concTerm 8
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 4 @=? concTerm 0
            pevalShiftBitsTerm (concTerm 15 :: Term (WordN 4)) 5 @=? concTerm 0

            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-5) @=? concTerm 15
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-4) @=? concTerm 15
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-3) @=? concTerm 15
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-2) @=? concTerm 15
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) (-1) @=? concTerm 15
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 0 @=? concTerm 15
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 1 @=? concTerm 14
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 2 @=? concTerm 12
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 3 @=? concTerm 8
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 4 @=? concTerm 0
            pevalShiftBitsTerm (concTerm 15 :: Term (IntN 4)) 5 @=? concTerm 0,
          testCase "shift 0" $ do
            pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 0 @=? ssymbTerm "a"
            pevalShiftBitsTerm (ssymbTerm "a" :: Term (IntN 4)) 0 @=? ssymbTerm "a",
          testCase "shift left bitsize" $ do
            pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 4 @=? concTerm 0
            pevalShiftBitsTerm (ssymbTerm "a" :: Term (IntN 4)) 4 @=? concTerm 0
            pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 5 @=? concTerm 0
            pevalShiftBitsTerm (ssymbTerm "a" :: Term (IntN 4)) 5 @=? concTerm 0,
          testCase "shift same direction twice" $ do
            pevalShiftBitsTerm (pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 1) 2
              @=? pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 3
            pevalShiftBitsTerm (pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) (-1)) (-2)
              @=? pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) (-3),
          testCase "shift symbolic" $ do
            pevalShiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
              @=? shiftBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
        ],
      testGroup
        "Rotate"
        [ testCase "On concrete" $ do
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-4) @=? concTerm 3
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-3) @=? concTerm 6
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-2) @=? concTerm 12
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) (-1) @=? concTerm 9
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 0 @=? concTerm 3
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 1 @=? concTerm 6
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 2 @=? concTerm 12
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 3 @=? concTerm 9
            pevalRotateBitsTerm (concTerm 3 :: Term (WordN 4)) 4 @=? concTerm 3,
          testCase "rotate 0" $ do
            pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 0 @=? ssymbTerm "a",
          testCase "rotate extra bits" $ do
            pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 4 @=? ssymbTerm "a"
            pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 5
              @=? pevalRotateBitsTerm (ssymbTerm "a") 1
            pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) (-1)
              @=? pevalRotateBitsTerm (ssymbTerm "a") 3,
          testCase "rotate twice" $ do
            pevalRotateBitsTerm (pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 1) 2
              @=? pevalRotateBitsTerm (ssymbTerm "a") 3,
          testCase "rotate symbolic" $ do
            pevalRotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
              @=? rotateBitsTerm (ssymbTerm "a" :: Term (WordN 4)) 2
        ]
    ]
