{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Pizza.Core.Data.Class.MergeableSpec where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as RWSTLazy
import qualified Control.Monad.RWS.Strict as RWSTStrict
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString.Char8 as C
import Data.Functor.Sum
import Data.Int
import Data.Word
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.TestUtils.Mergeable
import Pizza.TestUtils.SBool
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Mergeable for common types" $ do
    describe "Mergeable for SBool" $ do
      it "Mergeable for SBool should work" $ do
        let SimpleStrategy f = mergingStrategy :: MergingStrategy SBool SBool
        f (CBool True) (SSBool "a") (SSBool "b") `shouldBe` SSBool "a"
        f (CBool False) (SSBool "a") (SSBool "b") `shouldBe` SSBool "b"
        f (SSBool "a") (SSBool "b") (SSBool "c") `shouldBe` ITE (SSBool "a") (SSBool "b") (SSBool "c")
    describe "Mergeable for Bool" $ do
      prop "Mergeable for Bool should work" $ \(x :: Bool) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Integer" $ do
      prop "Mergeable for Integer should work" $ \(x :: Integer) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Char" $ do
      prop "Mergeable for Char should work" $ \(x :: Char) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Int" $ do
      prop "Mergeable for Int should work" $ \(x :: Int) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Int8" $ do
      prop "Mergeable for Int8 should work" $ \(x :: Int8) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Int16" $ do
      prop "Mergeable for Int16 should work" $ \(x :: Int16) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Int32" $ do
      prop "Mergeable for Int32 should work" $ \(x :: Int32) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Int64" $ do
      prop "Mergeable for Int64 should work" $ \(x :: Int64) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Word" $ do
      prop "Mergeable for Word should work" $ \(x :: Word) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Word8" $ do
      prop "Mergeable for Word8 should work" $ \(x :: Word8) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Word16" $ do
      prop "Mergeable for Word16 should work" $ \(x :: Word16) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Word32" $ do
      prop "Mergeable for Word32 should work" $ \(x :: Word32) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for Word64" $ do
      prop "Mergeable for Word64 should work" $ \(x :: Word64) ->
        testMergeableSimpleEquivClass x [DynamicSortedIdx x] [(SSBool "a", x, x, x)]
    describe "Mergeable for ()" $ do
      prop "Mergeable for () should work" $ \(x :: ()) ->
        testMergeableSimpleEquivClass x [] [(SSBool "a", x, x, x)]
    describe "Mergeable for ByteString" $ do
      prop "Mergeable for ByteString should work" $ \(x :: String) ->
        let b = C.pack x
         in testMergeableSimpleEquivClass b [DynamicSortedIdx b] [(SSBool "a", b, b, b)]
    describe "Mergeable for Either" $ do
      prop "Mergeable for Either Integer Integer should work" $ \(x :: Integer) -> do
        testMergeableSimpleEquivClass
          (Left x :: Either Integer Integer)
          [DynamicSortedIdx False, DynamicSortedIdx x]
          [(SSBool "a", Left x, Left x, Left x)]
        testMergeableSimpleEquivClass
          (Right x :: Either Integer Integer)
          [DynamicSortedIdx True, DynamicSortedIdx x]
          [(SSBool "a", Right x, Right x, Right x)]
      it "Mergeable for Either SBool SBool should work" $ do
        let (idxsL, SimpleStrategy fL) = resolveStrategy @SBool mergingStrategy (Left (SSBool "a") :: Either SBool SBool)
        idxsL `shouldBe` [DynamicSortedIdx False]
        fL (SSBool "a") (Left $ SSBool "b") (Left $ SSBool "c")
          `shouldBe` Left (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
        let (idxsR, SimpleStrategy fR) = resolveStrategy @SBool mergingStrategy (Right (SSBool "a") :: Either SBool SBool)
        idxsR `shouldBe` [DynamicSortedIdx True]
        fR (SSBool "a") (Right $ SSBool "b") (Right $ SSBool "c")
          `shouldBe` Right (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    describe "Mergeable for Maybe" $ do
      prop "Mergeable for Maybe Integer should work" $ \(x :: Integer) -> do
        testMergeableSimpleEquivClass
          (Nothing :: Maybe Integer)
          [DynamicSortedIdx False]
          [(SSBool "a", Nothing, Nothing, Nothing)]
        testMergeableSimpleEquivClass
          (Just x :: Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx x]
          [(SSBool "a", Just x, Just x, Just x)]
      it "Mergeable for Maybe SBool should work" $ do
        let (idxsJ, SimpleStrategy fJ) = resolveStrategy @SBool mergingStrategy (Just (SSBool "a") :: Maybe SBool)
        idxsJ `shouldBe` [DynamicSortedIdx True]
        fJ (SSBool "a") (Just $ SSBool "b") (Just $ SSBool "c")
          `shouldBe` Just (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    describe "Mergeable for List" $ do
      it "BuildStrategyList should work" $ do
        case buildStrategyList @SBool @Integer mergingStrategy [1, 2, 3] of
          StrategyList idxs _ -> do
            idxs
              `shouldBe` [ [DynamicSortedIdx (1 :: Integer)],
                           [DynamicSortedIdx (2 :: Integer)],
                           [DynamicSortedIdx (3 :: Integer)]
                         ]
      prop "Mergeable for List for ordered type should work" $ \(x :: [Integer]) -> do
        testMergeableSimpleEquivClass
          x
          [DynamicSortedIdx (length x), DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy x]
          [(SSBool "a", x, x, x)]
      prop "Mergeable for nested List for ordered type should work" $ \(x :: [[Integer]]) -> do
        testMergeableSimpleEquivClass
          x
          [DynamicSortedIdx (length x), DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy x]
          [(SSBool "a", x, x, x)]
      it "Mergeable for List for simple type should work" $ do
        testMergeableSimpleEquivClass
          ([] :: [SBool])
          [DynamicSortedIdx (0 :: Int)]
          [(SSBool "a", [], [], [])]
        testMergeableSimpleEquivClass
          [SSBool "a", SSBool "b"]
          [DynamicSortedIdx (2 :: Int)]
          [ ( SSBool "a",
              [SSBool "b", SSBool "c"],
              [SSBool "d", SSBool "e"],
              [ITE (SSBool "a") (SSBool "b") (SSBool "d"), ITE (SSBool "a") (SSBool "c") (SSBool "e")]
            )
          ]
    describe "Mergeable for (,)" $ do
      it "Mergeable for (,) should work" $ do
        testMergeableSimpleEquivClass
          ([1 :: Integer], [SSBool "b", SSBool "c"])
          [ DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
            DynamicSortedIdx (2 :: Int)
          ]
          [ ( SSBool "a",
              ([1], [SSBool "c", SSBool "d"]),
              ([1], [SSBool "f", SSBool "g"]),
              ( [1],
                [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                  ITE (SSBool "a") (SSBool "d") (SSBool "g")
                ]
              )
            )
          ]
    describe "Mergeable for (,,)" $ do
      it "Mergeable for (,,) should work" $ do
        testMergeableSimpleEquivClass
          ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d")
          [ DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
            DynamicSortedIdx (2 :: Int)
          ]
          [ ( SSBool "a",
              ([1], [SSBool "c", SSBool "d"], SSBool "e"),
              ([1], [SSBool "f", SSBool "g"], SSBool "h"),
              ( [1],
                [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                  ITE (SSBool "a") (SSBool "d") (SSBool "g")
                ],
                ITE (SSBool "a") (SSBool "e") (SSBool "h")
              )
            )
          ]
    describe "Mergeable for (,,,)" $ do
      it "Mergeable for (,,,) should work" $ do
        testMergeableSimpleEquivClass
          ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d", [SSBool "f"])
          [ DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx (1 :: Int)
          ]
          [ ( SSBool "a",
              ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"]),
              ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"]),
              ( [1],
                [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                  ITE (SSBool "a") (SSBool "d") (SSBool "g")
                ],
                ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                [ITE (SSBool "a") (SSBool "i") (SSBool "j")]
              )
            )
          ]
    describe "Mergeable for (,,,,)" $ do
      it "Mergeable for (,,,,) should work" $ do
        testMergeableSimpleEquivClass
          ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d", [SSBool "f"], [2 :: Integer, 3])
          [ DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3]
          ]
          [ ( SSBool "a",
              ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3]),
              ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3]),
              ( [1],
                [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                  ITE (SSBool "a") (SSBool "d") (SSBool "g")
                ],
                ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                [2, 3]
              )
            )
          ]
    describe "Mergeable for (,,,,,)" $ do
      it "Mergeable for (,,,,,) should work" $ do
        testMergeableSimpleEquivClass
          ([1 :: Integer], [SSBool "b", SSBool "c"], SSBool "d", [SSBool "f"], [2 :: Integer, 3], 2 :: Integer)
          [ DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3],
            DynamicSortedIdx (2 :: Integer)
          ]
          [ ( SSBool "a",
              ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3], 2),
              ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3], 2),
              ( [1],
                [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                  ITE (SSBool "a") (SSBool "d") (SSBool "g")
                ],
                ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                [2, 3],
                2
              )
            )
          ]
    describe "Mergeable for (,,,,,,)" $ do
      it "Mergeable for (,,,,,,) should work" $ do
        testMergeableSimpleEquivClass
          ( [1 :: Integer],
            [SSBool "b", SSBool "c"],
            SSBool "d",
            [SSBool "f"],
            [2 :: Integer, 3],
            2 :: Integer,
            Just (SSBool "a")
          )
          [ DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3],
            DynamicSortedIdx (2 :: Integer),
            DynamicSortedIdx True
          ]
          [ ( SSBool "a",
              ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3], 2, Just (SSBool "k")),
              ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3], 2, Just (SSBool "l")),
              ( [1],
                [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                  ITE (SSBool "a") (SSBool "d") (SSBool "g")
                ],
                ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                [2, 3],
                2,
                Just $ ITE (SSBool "a") (SSBool "k") (SSBool "l")
              )
            )
          ]
    describe "Mergeable for (,,,,,,,)" $ do
      it "Mergeable for (,,,,,,,) should work" $ do
        testMergeableSimpleEquivClass
          ( [1 :: Integer],
            [SSBool "b", SSBool "c"],
            SSBool "d",
            [SSBool "f"],
            [2 :: Integer, 3],
            2 :: Integer,
            Just (SSBool "a"),
            Left 1 :: Either Integer Integer
          )
          [ DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [1 :: Integer],
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx (1 :: Int),
            DynamicSortedIdx (2 :: Int),
            DynamicSortedIdx $ buildStrategyList @SBool mergingStrategy [2 :: Integer, 3],
            DynamicSortedIdx (2 :: Integer),
            DynamicSortedIdx True,
            DynamicSortedIdx False,
            DynamicSortedIdx (1 :: Integer)
          ]
          [ ( SSBool "a",
              ([1], [SSBool "c", SSBool "d"], SSBool "e", [SSBool "i"], [2, 3], 2, Just (SSBool "k"), Left 1),
              ([1], [SSBool "f", SSBool "g"], SSBool "h", [SSBool "j"], [2, 3], 2, Just (SSBool "l"), Left 1),
              ( [1],
                [ ITE (SSBool "a") (SSBool "c") (SSBool "f"),
                  ITE (SSBool "a") (SSBool "d") (SSBool "g")
                ],
                ITE (SSBool "a") (SSBool "e") (SSBool "h"),
                [ITE (SSBool "a") (SSBool "i") (SSBool "j")],
                [2, 3],
                2,
                Just $ ITE (SSBool "a") (SSBool "k") (SSBool "l"),
                Left 1
              )
            )
          ]
    describe "Mergeable for function" $ do
      let f1 :: Maybe SBool -> SBool = \case Just x -> x; Nothing -> (CBool True)
      let f2 :: Maybe SBool -> SBool = \case Just x -> (nots x); Nothing -> (CBool False)
      it "Mergeable for function with simply mergeable result" $ do
        case mergingStrategy :: MergingStrategy SBool (Maybe SBool -> SBool) of
          SimpleStrategy f -> do
            let r = f (SSBool "a") f1 f2
            r (Just (SSBool "x")) `shouldBe` ITE (SSBool "a") (SSBool "x") (Not (SSBool "x"))
            r Nothing `shouldBe` ITE (SSBool "a") (CBool True) (CBool False)
          _ -> expectationFailure "Bad mergeable strategy type"
      it "Mergeable for function with other mergeable result" $ do
        case mergingStrategy :: MergingStrategy SBool (Maybe SBool -> Integer) of
          NoStrategy -> return ()
          _ -> expectationFailure "Bad mergeable strategy type"
    describe "Mergeable for MaybeT" $ do
      prop "Mergeable for MaybeT Maybe Integer should work" $ \(x :: Integer) -> do
        testMergeableSimpleEquivClass
          (MaybeT Nothing :: MaybeT Maybe Integer)
          [DynamicSortedIdx False]
          [(SSBool "a", MaybeT Nothing, MaybeT Nothing, MaybeT Nothing)]
        testMergeableSimpleEquivClass
          (MaybeT $ Just Nothing :: MaybeT Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx False]
          [(SSBool "a", MaybeT $ Just Nothing, MaybeT $ Just Nothing, MaybeT $ Just Nothing)]
        testMergeableSimpleEquivClass
          (MaybeT $ Just $ Just x :: MaybeT Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx True, DynamicSortedIdx x]
          [(SSBool "a", MaybeT $ Just $ Just x, MaybeT $ Just $ Just x, MaybeT $ Just $ Just x)]
      it "Mergeable for MaybeT Maybe SBool should work" $ do
        let (idxsJ, SimpleStrategy fJ) =
              resolveStrategy @SBool
                mergingStrategy
                (MaybeT (Just (Just (SSBool "a"))) :: MaybeT Maybe SBool)
        idxsJ `shouldBe` [DynamicSortedIdx True, DynamicSortedIdx True]
        fJ (SSBool "a") (MaybeT $ Just $ Just $ SSBool "b") (MaybeT $ Just $ Just $ SSBool "c")
          `shouldBe` MaybeT (Just (Just (ITE (SSBool "a") (SSBool "b") (SSBool "c"))))
    describe "Mergeable for ExceptT" $ do
      prop "Mergeable for ExceptT Integer Maybe Integer should work" $ \(x :: Integer) -> do
        testMergeableSimpleEquivClass
          (ExceptT Nothing :: ExceptT Integer Maybe Integer)
          [DynamicSortedIdx False]
          [(SSBool "a", ExceptT Nothing, ExceptT Nothing, ExceptT Nothing)]
        testMergeableSimpleEquivClass
          (ExceptT $ Just $ Left x :: ExceptT Integer Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx False, DynamicSortedIdx x]
          [(SSBool "a", ExceptT $ Just $ Left x, ExceptT $ Just $ Left x, ExceptT $ Just $ Left x)]
        testMergeableSimpleEquivClass
          (ExceptT $ Just $ Right x :: ExceptT Integer Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx True, DynamicSortedIdx x]
          [(SSBool "a", ExceptT $ Just $ Right x, ExceptT $ Just $ Right x, ExceptT $ Just $ Right x)]
      it "Mergeable for ExceptT SBool Maybe SBool should work" $ do
        let (idxsJL, SimpleStrategy fJL) =
              resolveStrategy @SBool
                mergingStrategy
                (ExceptT (Just (Left (SSBool "a"))) :: ExceptT SBool Maybe SBool)
        idxsJL `shouldBe` [DynamicSortedIdx True, DynamicSortedIdx False]
        fJL (SSBool "a") (ExceptT $ Just $ Left $ SSBool "b") (ExceptT $ Just $ Left $ SSBool "c")
          `shouldBe` ExceptT (Just (Left (ITE (SSBool "a") (SSBool "b") (SSBool "c"))))
        let (idxsJR, SimpleStrategy fJR) =
              resolveStrategy @SBool
                mergingStrategy
                (ExceptT (Just (Right (SSBool "a"))) :: ExceptT SBool Maybe SBool)
        idxsJR `shouldBe` [DynamicSortedIdx True, DynamicSortedIdx True]
        fJR (SSBool "a") (ExceptT $ Just $ Right $ SSBool "b") (ExceptT $ Just $ Right $ SSBool "c")
          `shouldBe` ExceptT (Just (Right (ITE (SSBool "a") (SSBool "b") (SSBool "c"))))
    describe "Mergeable for StateT" $ do
      it "Mergeable for lazy StateT should work" $ do
        let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (StateLazy.StateT Integer (UnionMBase SBool) SBool)
        let st1 :: StateLazy.StateT Integer (UnionMBase SBool) SBool =
              StateLazy.StateT $ \(x :: Integer) -> mrgSingle (SSBool "a", x + 2)
        let st2 :: StateLazy.StateT Integer (UnionMBase SBool) SBool =
              StateLazy.StateT $ \(x :: Integer) -> mrgSingle (SSBool "b", x * 2)
        let st3 = s (SSBool "c") st1 st2
        StateLazy.runStateT st3 2 `shouldBe` mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
        StateLazy.runStateT st3 3 `shouldBe` mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
      it "Mergeable for strict StateT should work" $ do
        let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (StateStrict.StateT Integer (UnionMBase SBool) SBool)
        let st1 :: StateStrict.StateT Integer (UnionMBase SBool) SBool =
              StateStrict.StateT $ \(x :: Integer) -> mrgSingle (SSBool "a", x + 2)
        let st2 :: StateStrict.StateT Integer (UnionMBase SBool) SBool =
              StateStrict.StateT $ \(x :: Integer) -> mrgSingle (SSBool "b", x * 2)
        let st3 = s (SSBool "c") st1 st2
        StateStrict.runStateT st3 2 `shouldBe` mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
        StateStrict.runStateT st3 3 `shouldBe` mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
    describe "Mergeable for ContT" $ do
      it "Mergeable for ContT should work" $ do
        let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer))
        let c1 :: ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer) = ContT $ \f -> f (SSBool "a", 2)
        let c2 :: ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer) = ContT $ \f -> f (SSBool "b", 3)
        let c3 = s (SSBool "c") c1 c2
        runContT c3 (\(a, x) -> mrgIf (SSBool "p") (mrgSingle (a, x)) (mrgSingle (nots a, x + 1)))
          `shouldBe` mrgIf
            (SSBool "c")
            (mrgIf (SSBool "p") (mrgSingle (SSBool "a", 2)) (mrgSingle (Not $ SSBool "a", 3)))
            (mrgIf (SSBool "p") (mrgSingle (SSBool "b", 3)) (mrgSingle (Not $ SSBool "b", 4)))
    describe "Mergeable for RWST" $ do
      it "Mergeable for lazy RWST should work" $ do
        let SimpleStrategy s =
              mergingStrategy ::
                MergingStrategy
                  SBool
                  ( RWSTLazy.RWST
                      (Integer, SBool)
                      (Integer, SBool)
                      (Integer, SBool)
                      (UnionMBase SBool)
                      (Integer, SBool)
                  )
        let rws1 ::
              RWSTLazy.RWST
                (Integer, SBool)
                (Integer, SBool)
                (Integer, SBool)
                (UnionMBase SBool)
                (Integer, SBool) =
                RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                  mrgSingle ((ir + is, br &&~ bs), (ir - is, br ||~ bs), (ir * is, bs &&~ br))
        let rws2 ::
              RWSTLazy.RWST
                (Integer, SBool)
                (Integer, SBool)
                (Integer, SBool)
                (UnionMBase SBool)
                (Integer, SBool) =
                RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                  mrgSingle ((ir + is, br ||~ bs), (ir - is, br &&~ bs), (ir * is, bs ||~ br))
        let rws3 = s (SSBool "c") rws1 rws2

        let res1 :: UnionMBase SBool ((Integer, SBool), (Integer, SBool), (Integer, SBool)) =
              mrgIf
                (SSBool "c")
                (mrgSingle ((1, And (SSBool "a") (SSBool "b")), (-1, Or (SSBool "a") (SSBool "b")), (0, And (SSBool "b") (SSBool "a"))))
                (mrgSingle ((1, Or (SSBool "a") (SSBool "b")), (-1, And (SSBool "a") (SSBool "b")), (0, Or (SSBool "b") (SSBool "a"))))
        RWSTLazy.runRWST rws3 (0, SSBool "a") (1, SSBool "b") `shouldBe` res1
      it "Mergeable for strict RWST should work" $ do
        let SimpleStrategy s =
              mergingStrategy ::
                MergingStrategy
                  SBool
                  ( RWSTStrict.RWST
                      (Integer, SBool)
                      (Integer, SBool)
                      (Integer, SBool)
                      (UnionMBase SBool)
                      (Integer, SBool)
                  )
        let rws1 ::
              RWSTStrict.RWST
                (Integer, SBool)
                (Integer, SBool)
                (Integer, SBool)
                (UnionMBase SBool)
                (Integer, SBool) =
                RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                  mrgSingle ((ir + is, br &&~ bs), (ir - is, br ||~ bs), (ir * is, bs &&~ br))
        let rws2 ::
              RWSTStrict.RWST
                (Integer, SBool)
                (Integer, SBool)
                (Integer, SBool)
                (UnionMBase SBool)
                (Integer, SBool) =
                RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                  mrgSingle ((ir + is, br ||~ bs), (ir - is, br &&~ bs), (ir * is, bs ||~ br))
        let rws3 = s (SSBool "c") rws1 rws2

        let res1 :: UnionMBase SBool ((Integer, SBool), (Integer, SBool), (Integer, SBool)) =
              mrgIf
                (SSBool "c")
                (mrgSingle ((1, And (SSBool "a") (SSBool "b")), (-1, Or (SSBool "a") (SSBool "b")), (0, And (SSBool "b") (SSBool "a"))))
                (mrgSingle ((1, Or (SSBool "a") (SSBool "b")), (-1, And (SSBool "a") (SSBool "b")), (0, Or (SSBool "b") (SSBool "a"))))
        RWSTStrict.runRWST rws3 (0, SSBool "a") (1, SSBool "b") `shouldBe` res1
    describe "Mergeable for WriterT" $ do
      it "Mergeable for lazy WriterT should work" $ do
        let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (WriterLazy.WriterT Integer (UnionMBase SBool) SBool)
        let w1 :: WriterLazy.WriterT Integer (UnionMBase SBool) SBool =
              WriterLazy.WriterT $ mrgSingle (SSBool "a", 1)
        let w2 :: WriterLazy.WriterT Integer (UnionMBase SBool) SBool =
              WriterLazy.WriterT $ mrgSingle (SSBool "b", 2)
        let w3 :: WriterLazy.WriterT Integer (UnionMBase SBool) SBool =
              WriterLazy.WriterT $ mrgSingle (SSBool "c", 1)
        let w4 = s (SSBool "d") w1 w2
        let w5 = s (SSBool "d") w1 w3
        WriterLazy.runWriterT w4 `shouldBe` mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
        WriterLazy.runWriterT w5 `shouldBe` mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
      it "Mergeable for strict WriterT should work" $ do
        let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (WriterStrict.WriterT Integer (UnionMBase SBool) SBool)
        let w1 :: WriterStrict.WriterT Integer (UnionMBase SBool) SBool =
              WriterStrict.WriterT $ mrgSingle (SSBool "a", 1)
        let w2 :: WriterStrict.WriterT Integer (UnionMBase SBool) SBool =
              WriterStrict.WriterT $ mrgSingle (SSBool "b", 2)
        let w3 :: WriterStrict.WriterT Integer (UnionMBase SBool) SBool =
              WriterStrict.WriterT $ mrgSingle (SSBool "c", 1)
        let w4 = s (SSBool "d") w1 w2
        let w5 = s (SSBool "d") w1 w3
        WriterStrict.runWriterT w4 `shouldBe` mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
        WriterStrict.runWriterT w5 `shouldBe` mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
    describe "Mergeable for ReaderT" $ do
      it "Mergeable for ReaderT should work" $ do
        let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool (ReaderT Integer (UnionMBase SBool) Integer)
        let r1 :: ReaderT Integer (UnionMBase SBool) Integer =
              ReaderT $ \(x :: Integer) -> mrgSingle $ x + 2
        let r2 :: ReaderT Integer (UnionMBase SBool) Integer =
              ReaderT $ \(x :: Integer) -> mrgSingle $ x * 2
        let r3 = s (SSBool "c") r1 r2
        runReaderT r3 2 `shouldBe` mrgSingle 4
        runReaderT r3 3 `shouldBe` mrgIf (SSBool "c") (mrgSingle 5) (mrgSingle 6)
    describe "Mergeable for Identity" $ do
      prop "Mergeable for Identity Integer should work" $ \x -> do
        testMergeableSimpleEquivClass
          (Identity x :: Identity Integer)
          [DynamicSortedIdx x]
          [(SSBool "a", Identity x, Identity x, Identity x)]
      it "Mergeable for Identity SBool should work" $ do
        testMergeableSimpleEquivClass
          (Identity $ SSBool "a")
          []
          [ ( SSBool "a",
              Identity $ SSBool "b",
              Identity $ SSBool "c",
              Identity $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
            )
          ]
    describe "Mergeable for IdentityT" $ do
      prop "Mergeable for IdentityT Maybe Integer should work" $ \x -> do
        testMergeableSimpleEquivClass
          (IdentityT Nothing :: IdentityT Maybe Integer)
          [DynamicSortedIdx False]
          [(SSBool "a", IdentityT Nothing, IdentityT Nothing, IdentityT Nothing)]
        testMergeableSimpleEquivClass
          (IdentityT $ Just x :: IdentityT Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx x]
          [(SSBool "a", IdentityT $ Just x, IdentityT $ Just x, IdentityT $ Just x)]
      it "Mergeable for IdentityT Maybe SBool should work" $ do
        testMergeableSimpleEquivClass
          (IdentityT Nothing :: IdentityT Maybe SBool)
          [DynamicSortedIdx False]
          [(SSBool "a", IdentityT Nothing, IdentityT Nothing, IdentityT Nothing)]
        testMergeableSimpleEquivClass
          (IdentityT $ Just $ SSBool "a" :: IdentityT Maybe SBool)
          [DynamicSortedIdx True]
          [ ( SSBool "a",
              IdentityT $ Just $ SSBool "b",
              IdentityT $ Just $ SSBool "c",
              IdentityT $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
            )
          ]
    describe "Mergeable for Sum" $ do
      prop "Mergeable for Sum Maybe Maybe Integer should work" $ \x -> do
        testMergeableSimpleEquivClass
          (InL Nothing :: Sum Maybe Maybe Integer)
          [DynamicSortedIdx False, DynamicSortedIdx False]
          [(SSBool "a", InL Nothing, InL Nothing, InL Nothing)]
        testMergeableSimpleEquivClass
          (InL $ Just x :: Sum Maybe Maybe Integer)
          [DynamicSortedIdx False, DynamicSortedIdx True, DynamicSortedIdx x]
          [(SSBool "a", InL $ Just x, InL $ Just x, InL $ Just x)]
        testMergeableSimpleEquivClass
          (InR Nothing :: Sum Maybe Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx False]
          [(SSBool "a", InR Nothing, InR Nothing, InR Nothing)]
        testMergeableSimpleEquivClass
          (InR $ Just x :: Sum Maybe Maybe Integer)
          [DynamicSortedIdx True, DynamicSortedIdx True, DynamicSortedIdx x]
          [(SSBool "a", InR $ Just x, InR $ Just x, InR $ Just x)]
      it "Mergeable for Sum Maybe Maybe SBool should work" $ do
        testMergeableSimpleEquivClass
          (InL Nothing :: Sum Maybe Maybe SBool)
          [DynamicSortedIdx False, DynamicSortedIdx False]
          [(SSBool "a", InL Nothing, InL Nothing, InL Nothing)]
        testMergeableSimpleEquivClass
          (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
          [DynamicSortedIdx False, DynamicSortedIdx True]
          [ ( SSBool "a",
              InL $ Just $ SSBool "b",
              InL $ Just $ SSBool "c",
              InL $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
            )
          ]
        testMergeableSimpleEquivClass
          (InR Nothing :: Sum Maybe Maybe SBool)
          [DynamicSortedIdx True, DynamicSortedIdx False]
          [(SSBool "a", InR Nothing, InR Nothing, InR Nothing)]
        testMergeableSimpleEquivClass
          (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
          [DynamicSortedIdx True, DynamicSortedIdx True]
          [ ( SSBool "a",
              InR $ Just $ SSBool "b",
              InR $ Just $ SSBool "c",
              InR $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c")
            )
          ]

    describe "Mergeable for Ordering" $ do
      it "Mergeable for Ordering should work" $ do
        testMergeableSimpleEquivClass
          LT
          [DynamicSortedIdx False]
          [(SSBool "a", LT, LT, LT)]
        testMergeableSimpleEquivClass
          EQ
          [DynamicSortedIdx True, DynamicSortedIdx False]
          [(SSBool "a", EQ, EQ, EQ)]
        testMergeableSimpleEquivClass
          GT
          [DynamicSortedIdx True, DynamicSortedIdx True]
          [(SSBool "a", GT, GT, GT)]
