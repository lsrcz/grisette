module Main (main) where

import Grisette.Backend.SBV.Data.SMT.CEGISTests (cegisTests)
import Grisette.Backend.SBV.Data.SMT.LoweringTests
  ( loweringTests,
  )
import Grisette.Backend.SBV.Data.SMT.TermRewritingTests
  ( termRewritingTests,
  )
import Grisette.Core.Control.ExceptionTests (exceptionTests)
import Grisette.Core.Control.Monad.UnionMTests (unionMTests)
import Grisette.Core.Control.Monad.UnionTests (unionTests)
import qualified Grisette.Core.Data.BVTests
import qualified Grisette.Core.Data.Class.BoolTests
import Grisette.Core.Data.Class.EvaluateSymTests (evaluateSymTests)
import Grisette.Core.Data.Class.ExtractSymbolicsTests (extractSymbolicsTests)
import Grisette.Core.Data.Class.GPrettyTests (gprettyTests)
import Grisette.Core.Data.Class.GenSymTests (genSymTests)
import Grisette.Core.Data.Class.MergeableTests (mergeableTests)
import Grisette.Core.Data.Class.PlainUnionTests (plainUnionTests)
import Grisette.Core.Data.Class.SEqTests (seqTests)
import Grisette.Core.Data.Class.SOrdTests (sordTests)
import Grisette.Core.Data.Class.SafeDivisionTests (safeDivisionTests)
import Grisette.Core.Data.Class.SafeLinearArithTests (safeLinearArithTests)
import Grisette.Core.Data.Class.SafeSymRotateTests (safeSymRotateTests)
import Grisette.Core.Data.Class.SafeSymShiftTests (safeSymShiftTests)
import Grisette.Core.Data.Class.SimpleMergeableTests (simpleMergeableTests)
import Grisette.Core.Data.Class.SubstituteSymTests (substituteSymTests)
import Grisette.Core.Data.Class.SymRotateTests (symRotateTests)
import Grisette.Core.Data.Class.SymShiftTests (symShiftTests)
import Grisette.Core.Data.Class.ToConTests (toConTests)
import Grisette.Core.Data.Class.ToSymTests (toSymTests)
import Grisette.Core.Data.Class.TryMergeTests (tryMergeTests)
import Grisette.Core.Data.SomeBVTests (someBVTests)
import qualified Grisette.IR.SymPrim.Data.Prim.BVTests
import Grisette.IR.SymPrim.Data.Prim.BitsTests (bitsTests)
import qualified Grisette.IR.SymPrim.Data.Prim.BoolTests
import Grisette.IR.SymPrim.Data.Prim.IntegralTests
  ( integralTests,
  )
import Grisette.IR.SymPrim.Data.Prim.ModelTests (modelTests)
import Grisette.IR.SymPrim.Data.Prim.NumTests (numTests)
import qualified Grisette.IR.SymPrim.Data.Prim.TabularFunTests
import Grisette.IR.SymPrim.Data.SymPrimTests (symPrimTests)
import qualified Grisette.IR.SymPrim.Data.TabularFunTests
import Grisette.Lib.Control.ApplicativeTest (applicativeFunctionTests)
import Grisette.Lib.Control.Monad.ExceptTests
  ( monadExceptFunctionTests,
  )
import Grisette.Lib.Control.Monad.State.ClassTests
  ( monadStateClassTests,
  )
import Grisette.Lib.Control.Monad.Trans.ClassTests
  ( monadTransClassTests,
  )
import Grisette.Lib.Control.Monad.Trans.ExceptTests (exceptTests)
import Grisette.Lib.Control.Monad.Trans.State.LazyTests
  ( monadTransStateLazyTests,
  )
import Grisette.Lib.Control.Monad.Trans.State.StrictTests
  ( monadTransStateStrictTests,
  )
import Grisette.Lib.Control.MonadTests (monadFunctionTests)
import Grisette.Lib.Data.FoldableTests (foldableFunctionTests)
import Grisette.Lib.Data.FunctorTests (functorFunctionTests)
import Grisette.Lib.Data.ListTests (listTests)
import Grisette.Lib.Data.TraversableTests (traversableFunctionTests)
import Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    [ coreTests,
      irTests,
      sbvTests,
      libTests
    ]

coreTests :: Test
coreTests =
  testGroup
    "Grisette.Core"
    [ testGroup
        "Control"
        [ testGroup
            "Monad"
            [ unionMTests,
              unionTests
            ],
          exceptionTests
        ],
      testGroup
        "Data"
        [ testGroup
            "Class"
            [ Grisette.Core.Data.Class.BoolTests.boolTests,
              evaluateSymTests,
              extractSymbolicsTests,
              genSymTests,
              gprettyTests,
              mergeableTests,
              plainUnionTests,
              safeDivisionTests,
              safeLinearArithTests,
              safeSymShiftTests,
              safeSymRotateTests,
              seqTests,
              sordTests,
              simpleMergeableTests,
              substituteSymTests,
              symRotateTests,
              symShiftTests,
              toConTests,
              toSymTests,
              tryMergeTests
            ],
          Grisette.Core.Data.BVTests.bvTests,
          someBVTests
        ]
    ]

libTests :: Test
libTests =
  testGroup
    "Grisette.Lib"
    [ testGroup
        "Control"
        [ testGroup
            "Monad"
            [ monadExceptFunctionTests,
              testGroup
                "State"
                [ monadStateClassTests
                ],
              testGroup
                "Trans"
                [ exceptTests,
                  monadTransClassTests,
                  testGroup
                    "State"
                    [ monadTransStateLazyTests,
                      monadTransStateStrictTests
                    ]
                ]
            ],
          monadFunctionTests,
          applicativeFunctionTests
        ],
      testGroup
        "Data"
        [ foldableFunctionTests,
          traversableFunctionTests,
          functorFunctionTests,
          listTests
        ]
    ]

irTests :: Test
irTests =
  testGroup
    "Grisette.IR.SymPrim.Data"
    [ testGroup
        "Prim"
        [ bitsTests,
          Grisette.IR.SymPrim.Data.Prim.BoolTests.boolTests,
          Grisette.IR.SymPrim.Data.Prim.BVTests.bvTests,
          integralTests,
          modelTests,
          numTests,
          Grisette.IR.SymPrim.Data.Prim.TabularFunTests.tabularFunTests
        ],
      symPrimTests,
      Grisette.IR.SymPrim.Data.TabularFunTests.tabularFunTests
    ]

sbvTests :: Test
sbvTests =
  testGroup
    "Grisette.Backend.SBV.Data.SMT"
    [ cegisTests,
      loweringTests,
      termRewritingTests
    ]
