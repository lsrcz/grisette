{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.Core.Control.Monad.UnionSpec where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Trans.Writer.Strict as WriterStrict
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Lib.Control.Monad
import Pizza.TestUtils.SBool
import Test.Hspec

spec :: Spec
spec = do
  describe "getSingle" $ do
    it "getSingle should work" $ do
      getSingle (unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c") :: UnionMBase SBool SBool)
        `shouldBe` ITE (SSBool "a") (SSBool "b") (SSBool "c")
  describe "MonadUnion for MaybeT" $ do
    it "merge should work" $ do
      merge
        ( MaybeT
            ( unionIf (SSBool "a") (single $ Just $ SSBool "b") (single $ Just $ SSBool "c") ::
                UnionMBase SBool (Maybe SBool)
            )
        )
        `shouldBe` MaybeT (mrgSingle $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    it "mrgSingle should work" $ do
      (mrgSingle 1 :: MaybeT (UnionMBase SBool) Integer) `shouldBe` MaybeT (mrgSingle $ Just 1)
    it "mrgIf should work" $
      do
        mrgIf (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
        `shouldBe` MaybeT
          ( mrgSingle $ Just $ ITE (SSBool "a") (SSBool "b") (SSBool "c") ::
              UnionMBase SBool (Maybe SBool)
          )
  describe "MonadUnion for ExceptT" $ do
    it "merge should work" $ do
      merge
        ( ExceptT
            ( unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Left $ SSBool "c") ::
                UnionMBase SBool (Either SBool SBool)
            )
        )
        `shouldBe` ExceptT (mrgSingle $ Left $ ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    it "mrgSingle should work" $ do
      (mrgSingle 1 :: ExceptT SBool (UnionMBase SBool) Integer) `shouldBe` ExceptT (mrgSingle $ Right 1)
    it "mrgIf should work" $
      do
        mrgIf (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
        `shouldBe` ExceptT
          ( mrgSingle $ Right $ ITE (SSBool "a") (SSBool "b") (SSBool "c") ::
              UnionMBase SBool (Either SBool SBool)
          )

  describe "MonadUnion for StateT lazy" $ do
    it "merge should work" $ do
      let s :: StateLazy.StateT SBool (UnionMBase SBool) SBool =
            merge $ StateLazy.StateT $ \(x :: SBool) -> unionIf (SSBool "a") (single (x, Not x)) (single (Not x, x))
      StateLazy.runStateT s (SSBool "b")
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
            ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
          )
    it "mrgSingle should work" $ do
      let s :: StateLazy.StateT SBool (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
      StateLazy.runStateT s (SSBool "b") `shouldBe` mrgSingle (SSBool "x", SSBool "b")
    it "mrgIf should work" $ do
      let s :: StateLazy.StateT SBool (UnionMBase SBool) SBool =
            mrgIf
              (SSBool "a")
              (StateLazy.StateT $ \(x :: SBool) -> single (x, Not x))
              (StateLazy.StateT $ \(x :: SBool) -> single (Not x, x))
      StateLazy.runStateT s (SSBool "b")
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
            ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
          )
  describe "MonadUnion for StateT strict" $ do
    it "merge should work" $ do
      let s :: StateStrict.StateT SBool (UnionMBase SBool) SBool =
            merge $ StateStrict.StateT $ \(x :: SBool) -> unionIf (SSBool "a") (single (x, Not x)) (single (Not x, x))
      StateStrict.runStateT s (SSBool "b")
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
            ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
          )
    it "mrgSingle should work" $ do
      let s :: StateStrict.StateT SBool (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
      StateStrict.runStateT s (SSBool "b") `shouldBe` mrgSingle (SSBool "x", SSBool "b")
    it "mrgIf should work" $ do
      let s :: StateStrict.StateT SBool (UnionMBase SBool) SBool =
            mrgIf
              (SSBool "a")
              (StateStrict.StateT $ \(x :: SBool) -> single (x, Not x))
              (StateStrict.StateT $ \(x :: SBool) -> single (Not x, x))
      StateStrict.runStateT s (SSBool "b")
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"),
            ITE (SSBool "a") (Not $ SSBool "b") (SSBool "b")
          )
  describe "MonadUnion for WriterT lazy" $ do
    it "merge should work" $ do
      let s :: WriterLazy.WriterT [SBool] (UnionMBase SBool) SBool =
            merge $
              WriterLazy.WriterT $
                unionIf
                  (SSBool "a")
                  (single (SSBool "b", [SSBool "c"]))
                  (single (SSBool "d", [SSBool "e"]))
      WriterLazy.runWriterT s
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
            [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
          )
    it "mrgSingle should work" $ do
      let s :: WriterLazy.WriterT [SBool] (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
      WriterLazy.runWriterT s `shouldBe` mrgSingle (SSBool "x", [])
    it "mrgIf should work" $ do
      let s :: WriterLazy.WriterT [SBool] (UnionMBase SBool) SBool =
            mrgIf
              (SSBool "a")
              (WriterLazy.WriterT $ single (SSBool "b", [SSBool "c"]))
              (WriterLazy.WriterT $ single (SSBool "d", [SSBool "e"]))
      WriterLazy.runWriterT s
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
            [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
          )
  describe "MonadUnion for WriterT Strict" $ do
    it "merge should work" $ do
      let s :: WriterStrict.WriterT [SBool] (UnionMBase SBool) SBool =
            merge $
              WriterStrict.WriterT $
                unionIf
                  (SSBool "a")
                  (single (SSBool "b", [SSBool "c"]))
                  (single (SSBool "d", [SSBool "e"]))
      WriterStrict.runWriterT s
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
            [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
          )
    it "mrgSingle should work" $ do
      let s :: WriterStrict.WriterT [SBool] (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
      WriterStrict.runWriterT s `shouldBe` mrgSingle (SSBool "x", [])
    it "mrgIf should work" $ do
      let s :: WriterStrict.WriterT [SBool] (UnionMBase SBool) SBool =
            mrgIf
              (SSBool "a")
              (WriterStrict.WriterT $ single (SSBool "b", [SSBool "c"]))
              (WriterStrict.WriterT $ single (SSBool "d", [SSBool "e"]))
      WriterStrict.runWriterT s
        `shouldBe` mrgSingle
          ( ITE (SSBool "a") (SSBool "b") (SSBool "d"),
            [ITE (SSBool "a") (SSBool "c") (SSBool "e")]
          )
  describe "MonadUnion for ReaderT" $ do
    it "merge should work" $ do
      let s :: ReaderT SBool (UnionMBase SBool) SBool =
            merge $ ReaderT $ \(x :: SBool) -> unionIf (SSBool "a") (single x) (single $ Not x)
      runReaderT s (SSBool "b")
        `shouldBe` mrgSingle
          (ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"))
    it "mrgSingle should work" $ do
      let s :: ReaderT SBool (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
      runReaderT s (SSBool "b") `shouldBe` mrgSingle (SSBool "x")
    it "mrgIf should work" $ do
      let s :: ReaderT SBool (UnionMBase SBool) SBool =
            mrgIf
              (SSBool "a")
              (ReaderT $ \(x :: SBool) -> single x)
              (ReaderT $ \(x :: SBool) -> single $ Not x)
      runReaderT s (SSBool "b")
        `shouldBe` mrgSingle
          (ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"))
  describe "MonadUnion for IdentityT" $ do
    it "merge should work" $ do
      let s :: IdentityT (UnionMBase SBool) SBool =
            merge $
              IdentityT $
                unionIf
                  (SSBool "a")
                  (single $ SSBool "b")
                  (single $ SSBool "c")
      runIdentityT s `shouldBe` mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    it "mrgSingle should work" $ do
      let s :: IdentityT (UnionMBase SBool) SBool = mrgSingle (SSBool "x")
      runIdentityT s `shouldBe` mrgSingle (SSBool "x")
    it "mrgIf should work" $ do
      let s :: IdentityT (UnionMBase SBool) SBool =
            mrgIf
              (SSBool "a")
              (IdentityT $ single (SSBool "b"))
              (IdentityT $ single (SSBool "c"))
      runIdentityT s `shouldBe` mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
  describe ">>=~" $ do
    it ">>=~ should work" $ do
      unionIf (SSBool "a") (single $ -1) (single 1)
        >>=~ (\x -> return $ x * x)
        `shouldBe` (mrgSingle 1 :: UnionMBase SBool Integer)
