module Main (main) where

{-
import Grisette.Core.Control.ExceptionTests
import Grisette.Core.Control.Monad.UnionMBaseTests
import Grisette.Core.Data.Class.BoolTests
import Grisette.Core.Data.Class.EvaluateSymTests
import Grisette.Core.Data.Class.ExtractSymbolicsTests
import Grisette.Core.Data.Class.GenSymTests
import Grisette.Core.Data.Class.MergeableTests
import Grisette.Core.Data.Class.SEqTests
import Grisette.Core.Data.Class.SOrdTests
import Grisette.Core.Data.Class.SimpleMergeableTests
import Grisette.Core.Data.Class.ToConTests
import Grisette.Core.Data.Class.ToSymTests
import Grisette.Core.Data.Class.UnionLikeTests
import Grisette.Core.Data.UnionBaseTests
import Grisette.Lib.Control.Monad.ExceptTests
import Grisette.Lib.Control.Monad.TransTests
import Grisette.Lib.Control.MonadTests
import Grisette.Lib.Data.FoldableTests
import Grisette.Lib.Data.TraversableTests
-}

import Grisette.Backend.SBV.Data.SMT.CEGISTests (cegisTests)
import Grisette.Backend.SBV.Data.SMT.LoweringTests
  ( loweringTests,
  )
import Grisette.Backend.SBV.Data.SMT.TermRewritingTests
  ( termRewritingTests,
  )
import Grisette.Core.Control.Monad.UnionMTests (unionMTests)
import qualified Grisette.Core.Data.BVTests
import Grisette.Core.Data.Class.GPrettyTests (gprettyTests)
import qualified Grisette.IR.SymPrim.Data.Prim.BVTests
import Grisette.IR.SymPrim.Data.Prim.BitsTests (bitsTests)
import Grisette.IR.SymPrim.Data.Prim.BoolTests (boolTests)
import Grisette.IR.SymPrim.Data.Prim.IntegralTests
  ( integralTests,
  )
import Grisette.IR.SymPrim.Data.Prim.ModelTests (modelTests)
import Grisette.IR.SymPrim.Data.Prim.NumTests (numTests)
import qualified Grisette.IR.SymPrim.Data.Prim.TabularFunTests
import Grisette.IR.SymPrim.Data.SymPrimTests (symPrimTests)
import qualified Grisette.IR.SymPrim.Data.TabularFunTests
import Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    [ coreTests,
      irTests,
      sbvTests
    ]

coreTests :: Test
coreTests =
  testGroup
    "core"
    [ testGroup
        "Grisette.Core.Control.Monad.UnionM"
        [unionMTests],
      testGroup
        "Grisette.Core.Data"
        [ testGroup
            "Class"
            [gprettyTests],
          Grisette.Core.Data.BVTests.bvTests
        ]
    ]

{-
coreTests :: Test
coreTests =
  testGroup
    "grisette-core"
    [ testGroup
        "Grisette"
        [ testGroup
            "Core"
            [ testGroup
                "Control"
                [ testGroup
                    "Monad"
                    [ unionMBaseTests
                    ],
                  exceptionTests
                ],
              testGroup
                "Data"
                [ testGroup
                    "Class"
                    [ boolTests,
                      gevaluateSymTests,
                      gextractSymbolicsTests,
                      genSymTests,
                      mergeableTests,
                      seqTests,
                      simpleMergeableTests,
                      sordTests,
                      toConTests,
                      toSymTests,
                      unionLikeTests
                    ],
                  unionBaseTests
                ]
            ],
          testGroup
            "Lib"
            [ testGroup
                "Control"
                [ testGroup
                    "Monad"
                    [ monadExceptFunctionTests,
                      monadTransFunctionTests
                    ],
                  monadFunctionTests
                ],
              testGroup
                "Data"
                [ foldableFunctionTests,
                  traversableFunctionTests
                ]
            ]
        ]
    ]
-}

irTests :: Test
irTests =
  testGroup
    "grisette-symir"
    [ testGroup
        "Grisette.IR.SymPrim.Data"
        [ testGroup
            "Prim"
            [ bitsTests,
              boolTests,
              Grisette.IR.SymPrim.Data.Prim.BVTests.bvTests,
              integralTests,
              modelTests,
              numTests,
              Grisette.IR.SymPrim.Data.Prim.TabularFunTests.tabularFunTests
            ],
          symPrimTests,
          Grisette.IR.SymPrim.Data.TabularFunTests.tabularFunTests
        ]
    ]

sbvTests :: Test
sbvTests =
  testGroup
    "Grisette.Backend.SBV.Data.SMT"
    [ cegisTests,
      loweringTests,
      termRewritingTests
    ]
