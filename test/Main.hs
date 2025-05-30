module Main (main) where

import Grisette.Backend.CEGISTests (cegisTests)
import Grisette.Backend.LoweringTests
  ( loweringTests,
  )
import Grisette.Backend.TermRewritingTests
  ( termRewritingTests,
  )
import Grisette.Core.Control.ExceptionTests (exceptionTests)
import Grisette.Core.Control.Monad.UnionTests (unionTests)
import Grisette.Core.Data.Class.BitCastTests (bitCastTests)
import qualified Grisette.Core.Data.Class.BoolTests
import Grisette.Core.Data.Class.EvalSymTests (evalSymTests)
import Grisette.Core.Data.Class.ExtractSymTests (extractSymTests)
import Grisette.Core.Data.Class.GenSymTests (genSymTests)
import Grisette.Core.Data.Class.MergeableTests (mergeableTests)
import Grisette.Core.Data.Class.PPrintTests (pprintTests)
import Grisette.Core.Data.Class.SafeDivTests (safeDivTests)
import Grisette.Core.Data.Class.SafeLinearArithTests (safeLinearArithTests)
import Grisette.Core.Data.Class.SafeSymRotateTests (safeSymRotateTests)
import Grisette.Core.Data.Class.SafeSymShiftTests (safeSymShiftTests)
import Grisette.Core.Data.Class.SimpleMergeableTests (simpleMergeableTests)
import Grisette.Core.Data.Class.SubstSymTests (substSymTests)
import Grisette.Core.Data.Class.SymEqTests (seqTests)
import Grisette.Core.Data.Class.SymFiniteBitsTests (symFiniteBitsTests)
import Grisette.Core.Data.Class.SymOrdTests (sordTests)
import Grisette.Core.Data.Class.SymRotateTests (symRotateTests)
import Grisette.Core.Data.Class.SymShiftTests (symShiftTests)
import Grisette.Core.Data.Class.ToConTests (toConTests)
import Grisette.Core.Data.Class.ToSymTests (toSymTests)
import Grisette.Core.Data.Class.TryMergeTests (tryMergeTests)
import Grisette.Core.Data.Class.UnionViewTests (unionViewTests)
import Grisette.Core.Data.UnionBaseTests (unionBaseTests)
import Grisette.Core.TH.DerivationTest (derivationTest)
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
import Grisette.SymPrim.AlgRealTests (algRealTests)
import qualified Grisette.SymPrim.BVTests
import Grisette.SymPrim.FPTests (fpTests)
import Grisette.SymPrim.GeneralFunTests (generalFunTests)
import qualified Grisette.SymPrim.Prim.BVTests
import Grisette.SymPrim.Prim.BitsTests (bitsTests)
import qualified Grisette.SymPrim.Prim.BoolTests
import Grisette.SymPrim.Prim.ConcurrentTests (concurrentTests)
import Grisette.SymPrim.Prim.IntegralTests (integralTests)
import Grisette.SymPrim.Prim.ModelTests (modelTests)
import Grisette.SymPrim.Prim.NumTests (numTests)
import qualified Grisette.SymPrim.Prim.SerializationTests
import qualified Grisette.SymPrim.Prim.TabularFunTests
import Grisette.SymPrim.QuantifierTests (quantifierTests)
import Grisette.SymPrim.SomeBVTests (someBVTests)
import Grisette.SymPrim.SymGeneralFunTests (symGeneralFunTests)
import Grisette.SymPrim.SymPrimTests (symPrimTests)
import qualified Grisette.SymPrim.TabularFunTests
import Grisette.Unified.EvalModeTest (evalModeTest)
import Grisette.Unified.GetDataTest (getDataTest)
import Grisette.Unified.UnifiedClassesTest (unifiedClassesTest)
import Grisette.Unified.UnifiedConstructorTest (unifiedConstructorTest)
import Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    [ coreTests,
      irTests,
      sbvTests,
      libTests,
      unifiedTests
    ]

coreTests :: Test
coreTests =
  testGroup
    "Grisette.Core"
    [ testGroup
        "Control"
        [ testGroup
            "Monad"
            [ unionTests
            ],
          exceptionTests
        ],
      testGroup
        "Data"
        [ testGroup
            "Class"
            [ bitCastTests,
              Grisette.Core.Data.Class.BoolTests.boolTests,
              evalSymTests,
              extractSymTests,
              genSymTests,
              pprintTests,
              mergeableTests,
              unionViewTests,
              safeDivTests,
              safeLinearArithTests,
              safeSymShiftTests,
              safeSymRotateTests,
              seqTests,
              sordTests,
              simpleMergeableTests,
              substSymTests,
              symFiniteBitsTests,
              symRotateTests,
              symShiftTests,
              toConTests,
              toSymTests,
              tryMergeTests
            ],
          unionBaseTests
        ],
      testGroup "TH" [derivationTest]
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
    "Grisette.SymPrim"
    [ testGroup
        "Prim"
        [ concurrentTests,
          bitsTests,
          Grisette.SymPrim.Prim.BoolTests.boolTests,
          Grisette.SymPrim.Prim.BVTests.bvTests,
          integralTests,
          modelTests,
          numTests,
          Grisette.SymPrim.Prim.TabularFunTests.tabularFunTests,
          Grisette.SymPrim.Prim.SerializationTests.serializationTests
        ],
      symPrimTests,
      Grisette.SymPrim.TabularFunTests.tabularFunTests,
      Grisette.SymPrim.BVTests.bvTests,
      algRealTests,
      generalFunTests,
      symGeneralFunTests,
      someBVTests,
      fpTests,
      quantifierTests
    ]

sbvTests :: Test
sbvTests =
  testGroup
    "Grisette.Backend"
    [ cegisTests,
      loweringTests,
      termRewritingTests
    ]

unifiedTests :: Test
unifiedTests =
  testGroup
    "Grisette.Unified"
    [ evalModeTest,
      getDataTest,
      unifiedConstructorTest,
      unifiedClassesTest
    ]
