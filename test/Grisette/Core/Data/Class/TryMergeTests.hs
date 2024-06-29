{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Grisette.Core.Data.Class.TryMergeTests (tryMergeTests) where

import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity (IdentityT (IdentityT))
import qualified Control.Monad.RWS.Lazy as RWSTLazy
import qualified Control.Monad.RWS.Strict as RWSTStrict
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Grisette
  ( ITEOp (symIte),
    Mergeable (rootStrategy),
    SymBranching (mrgIfPropagatedStrategy),
    TryMerge,
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.Core.Control.Monad.Union (Union (UMrg))
import Grisette.Internal.Core.Data.UnionBase (UnionBase (UnionSingle))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data TryMergeInstanceTest where
  TryMergeInstanceTest ::
    forall u a.
    (TryMerge u, Mergeable a, Show (u a), Eq (u a)) =>
    { testName :: String,
      testUnmerged :: u a,
      testMerged :: u a
    } ->
    TryMergeInstanceTest

unmergedUnion :: Union SymInteger
unmergedUnion =
  mrgIfPropagatedStrategy
    "a"
    (return "b")
    (return "c")

tryMergeTests :: Test
tryMergeTests =
  testGroup
    "TryMerge"
    [ testCase "mrgSingle" $ do
        let actual = mrgSingle 1 :: Union Integer
        actual @?= (UMrg rootStrategy (UnionSingle 1)),
      testCase "mrgSingle" $ do
        let actual = mrgSingle 1 :: Union Integer
        actual @?= (UMrg rootStrategy (UnionSingle 1)),
      testCase "tryMerge" $ do
        let actual = tryMerge $ return 1 :: Union Integer
        actual @?= (UMrg rootStrategy (UnionSingle 1)),
      testGroup "Instances" $ do
        test <-
          [ TryMergeInstanceTest
              { testName = "MaybeT",
                testUnmerged = MaybeT $ Just <$> unmergedUnion,
                testMerged = MaybeT (mrgSingle $ Just $ symIte "a" "b" "c")
              },
            TryMergeInstanceTest
              { testName = "ExceptT",
                testUnmerged =
                  ExceptT $ Right <$> unmergedUnion ::
                    ExceptT SymInteger Union SymInteger,
                testMerged = ExceptT (mrgSingle $ Right $ symIte "a" "b" "c")
              },
            TryMergeInstanceTest
              { testName = "ReaderT",
                testUnmerged =
                  runReaderT (ReaderT $ \r -> (r +) <$> unmergedUnion) "x",
                testMerged = mrgSingle (symIte "a" ("x" + "b") ("x" + "c"))
              },
            TryMergeInstanceTest
              { testName = "Lazy StateT",
                testUnmerged =
                  StateLazy.runStateT
                    (StateLazy.StateT $ \s -> (,s) <$> unmergedUnion)
                    "x" ::
                    Union (SymInteger, SymInteger),
                testMerged = mrgSingle (symIte "a" "b" "c", "x")
              },
            TryMergeInstanceTest
              { testName = "Strict StateT",
                testUnmerged =
                  StateStrict.runStateT
                    (StateStrict.StateT $ \s -> (,s) <$> unmergedUnion)
                    "x" ::
                    Union (SymInteger, SymInteger),
                testMerged = mrgSingle (symIte "a" "b" "c", "x")
              },
            TryMergeInstanceTest
              { testName = "Lazy WriterT",
                testUnmerged =
                  WriterLazy.runWriterT
                    ( WriterLazy.WriterT $
                        (\x -> (x, x + 1)) <$> unmergedUnion
                    ) ::
                    Union (SymInteger, SymInteger),
                testMerged =
                  mrgSingle (symIte "a" "b" "c", symIte "a" ("b" + 1) ("c" + 1))
              },
            TryMergeInstanceTest
              { testName = "Strict WriterT",
                testUnmerged =
                  WriterStrict.runWriterT
                    ( WriterStrict.WriterT $
                        (\x -> (x, x + 1)) <$> unmergedUnion
                    ) ::
                    Union (SymInteger, SymInteger),
                testMerged =
                  mrgSingle (symIte "a" "b" "c", symIte "a" ("b" + 1) ("c" + 1))
              },
            TryMergeInstanceTest
              { testName = "Lazy RWST",
                testUnmerged =
                  RWSTLazy.runRWST
                    ( RWSTLazy.RWST $
                        \r s -> (,s,r) <$> unmergedUnion
                    )
                    "r"
                    "s" ::
                    Union (SymInteger, SymInteger, SymInteger),
                testMerged = mrgSingle (symIte "a" "b" "c", "s", "r")
              },
            TryMergeInstanceTest
              { testName = "Strict RWST",
                testUnmerged =
                  RWSTStrict.runRWST
                    ( RWSTStrict.RWST $
                        \r s -> (,s,r) <$> unmergedUnion
                    )
                    "r"
                    "s" ::
                    Union (SymInteger, SymInteger, SymInteger),
                testMerged = mrgSingle (symIte "a" "b" "c", "s", "r")
              },
            TryMergeInstanceTest
              { testName = "IdentityT",
                testUnmerged = IdentityT unmergedUnion,
                testMerged = IdentityT $ mrgSingle $ symIte "a" "b" "c"
              },
            TryMergeInstanceTest
              { testName = "ContT",
                testUnmerged =
                  runContT
                    (ContT $ \c -> unmergedUnion >>= c)
                    (\x -> mrgSingle $ x + 1),
                testMerged = mrgSingle $ symIte "a" ("b" + 1) ("c" + 1)
              }
            ]
        case test of
          TryMergeInstanceTest name unmerged merged ->
            [ testCase name $ do
                let actual = tryMerge unmerged
                actual @?= merged
            ]
    ]
