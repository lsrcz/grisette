{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.SimpleMergeableTests where

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
import qualified Data.Monoid as Monoid
import GHC.Generics
import Generics.Deriving
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

newtype AndMonoidSBool = AndMonoidSBool SBool
  deriving (Show, Generic, Eq)
  deriving (GMergeable SBool) via (Default AndMonoidSBool)

instance Semigroup AndMonoidSBool where
  (AndMonoidSBool a) <> (AndMonoidSBool b) = AndMonoidSBool (And a b)

instance Monoid AndMonoidSBool where
  mempty = AndMonoidSBool $ CBool True

simpleMergeableTests :: TestTree
simpleMergeableTests =
  testGroup
    "SimpleMergeableTests"
    [ testGroup
        "SimpleMergeable for common types"
        [ testCase "SBool" $ do
            gmrgIte (SSBool "a") (SSBool "b") (SSBool "c") @=? ITE (SSBool "a") (SSBool "b") (SSBool "c"),
          testCase "()" $ do
            gmrgIte (SSBool "a") () () @=? (),
          testCase "(SBool, SBool)" $ do
            gmrgIte (SSBool "a") (SSBool "b", SSBool "d") (SSBool "c", SSBool "e")
              @=? (ITE (SSBool "a") (SSBool "b") (SSBool "c"), ITE (SSBool "a") (SSBool "d") (SSBool "e")),
          testCase "(SBool, SBool, SBool)" $ do
            gmrgIte (SSBool "a") (SSBool "b", SSBool "d", SSBool "f") (SSBool "c", SSBool "e", SSBool "g")
              @=? ( ITE (SSBool "a") (SSBool "b") (SSBool "c"),
                    ITE (SSBool "a") (SSBool "d") (SSBool "e"),
                    ITE (SSBool "a") (SSBool "f") (SSBool "g")
                  ),
          testCase "(SBool, SBool, SBool, SBool)" $ do
            gmrgIte
              (SSBool "a")
              (SSBool "b", SSBool "d", SSBool "f", SSBool "h")
              (SSBool "c", SSBool "e", SSBool "g", SSBool "i")
              @=? ( ITE (SSBool "a") (SSBool "b") (SSBool "c"),
                    ITE (SSBool "a") (SSBool "d") (SSBool "e"),
                    ITE (SSBool "a") (SSBool "f") (SSBool "g"),
                    ITE (SSBool "a") (SSBool "h") (SSBool "i")
                  ),
          testCase "(SBool, SBool, SBool, SBool, SBool)" $ do
            gmrgIte
              (SSBool "a")
              (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j")
              (SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k")
              @=? ( ITE (SSBool "a") (SSBool "b") (SSBool "c"),
                    ITE (SSBool "a") (SSBool "d") (SSBool "e"),
                    ITE (SSBool "a") (SSBool "f") (SSBool "g"),
                    ITE (SSBool "a") (SSBool "h") (SSBool "i"),
                    ITE (SSBool "a") (SSBool "j") (SSBool "k")
                  ),
          testCase "(SBool, SBool, SBool, SBool, SBool, SBool)" $ do
            gmrgIte
              (SSBool "a")
              (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l")
              (SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m")
              @=? ( ITE (SSBool "a") (SSBool "b") (SSBool "c"),
                    ITE (SSBool "a") (SSBool "d") (SSBool "e"),
                    ITE (SSBool "a") (SSBool "f") (SSBool "g"),
                    ITE (SSBool "a") (SSBool "h") (SSBool "i"),
                    ITE (SSBool "a") (SSBool "j") (SSBool "k"),
                    ITE (SSBool "a") (SSBool "l") (SSBool "m")
                  ),
          testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
            gmrgIte
              (SSBool "a")
              (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n")
              (SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m", SSBool "o")
              @=? ( ITE (SSBool "a") (SSBool "b") (SSBool "c"),
                    ITE (SSBool "a") (SSBool "d") (SSBool "e"),
                    ITE (SSBool "a") (SSBool "f") (SSBool "g"),
                    ITE (SSBool "a") (SSBool "h") (SSBool "i"),
                    ITE (SSBool "a") (SSBool "j") (SSBool "k"),
                    ITE (SSBool "a") (SSBool "l") (SSBool "m"),
                    ITE (SSBool "a") (SSBool "n") (SSBool "o")
                  ),
          testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
            gmrgIte
              (SSBool "a")
              (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n", SSBool "p")
              (SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m", SSBool "o", SSBool "q")
              @=? ( ITE (SSBool "a") (SSBool "b") (SSBool "c"),
                    ITE (SSBool "a") (SSBool "d") (SSBool "e"),
                    ITE (SSBool "a") (SSBool "f") (SSBool "g"),
                    ITE (SSBool "a") (SSBool "h") (SSBool "i"),
                    ITE (SSBool "a") (SSBool "j") (SSBool "k"),
                    ITE (SSBool "a") (SSBool "l") (SSBool "m"),
                    ITE (SSBool "a") (SSBool "n") (SSBool "o"),
                    ITE (SSBool "a") (SSBool "p") (SSBool "q")
                  ),
          testCase "SBool -> SBool" $ do
            let f = gmrgIte (SSBool "a") Not (And (SSBool "b"))
            f (SSBool "c") @=? ITE (SSBool "a") (Not $ SSBool "c") (And (SSBool "b") (SSBool "c")),
          testCase "MaybeT (UnionMBase SBool) SBool" $ do
            let l :: MaybeT (UnionMBase SBool) SBool =
                  MaybeT (mrgIf (SSBool "b") (mrgSingle Nothing) (mrgSingle $ Just $ SSBool "c"))
            let r :: MaybeT (UnionMBase SBool) SBool =
                  MaybeT (mrgIf (SSBool "d") (mrgSingle Nothing) (mrgSingle $ Just $ SSBool "e"))
            let res :: MaybeT (UnionMBase SBool) SBool =
                  MaybeT
                    ( mrgIf
                        (SSBool "a")
                        (mrgIf (SSBool "b") (mrgSingle Nothing) (mrgSingle $ Just $ SSBool "c"))
                        (mrgIf (SSBool "d") (mrgSingle Nothing) (mrgSingle $ Just $ SSBool "e"))
                    )
            gmrgIte (SSBool "a") l r @=? res
            gmrgIte1 (SSBool "a") l r @=? res
            mrgIf (SSBool "a") l r @=? res,
          testCase "ExceptT SBool (UnionMBase SBool) SBool" $ do
            let l :: ExceptT SBool (UnionMBase SBool) SBool =
                  ExceptT (mrgIf (SSBool "b") (mrgSingle $ Left $ SSBool "c") (mrgSingle $ Right $ SSBool "d"))
            let r = ExceptT (mrgIf (SSBool "e") (mrgSingle $ Left $ SSBool "f") (mrgSingle $ Right $ SSBool "g"))
            let res =
                  ExceptT
                    ( mrgIf
                        (SSBool "a")
                        (mrgIf (SSBool "b") (mrgSingle $ Left $ SSBool "c") (mrgSingle $ Right $ SSBool "d"))
                        (mrgIf (SSBool "e") (mrgSingle $ Left $ SSBool "f") (mrgSingle $ Right $ SSBool "g"))
                    )
            gmrgIte (SSBool "a") l r @=? res
            gmrgIte1 (SSBool "a") l r @=? res
            mrgIf (SSBool "a") l r @=? res,
          testGroup
            "StateT Integer (UnionMBase SBool) SBool"
            [ testCase "Lazy" $ do
                let st1 :: StateLazy.StateT Integer (UnionMBase SBool) SBool =
                      StateLazy.StateT $ \(x :: Integer) -> mrgSingle (SSBool "a", x + 2)
                let st2 :: StateLazy.StateT Integer (UnionMBase SBool) SBool =
                      StateLazy.StateT $ \(x :: Integer) -> mrgSingle (SSBool "b", x * 2)
                let st3 = gmrgIte (SSBool "c") st1 st2
                let st31 = gmrgIte1 (SSBool "c") st1 st2
                let st3u1 = mrgIf (SSBool "c") st1 st2
                StateLazy.runStateT st3 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateLazy.runStateT st3 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
                StateLazy.runStateT st31 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateLazy.runStateT st31 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
                StateLazy.runStateT st3u1 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateLazy.runStateT st3u1 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6)),
              testCase "Strict" $ do
                let st1 :: StateStrict.StateT Integer (UnionMBase SBool) SBool =
                      StateStrict.StateT $ \(x :: Integer) -> mrgSingle (SSBool "a", x + 2)
                let st2 :: StateStrict.StateT Integer (UnionMBase SBool) SBool =
                      StateStrict.StateT $ \(x :: Integer) -> mrgSingle (SSBool "b", x * 2)
                let st3 = gmrgIte (SSBool "c") st1 st2
                let st31 = gmrgIte1 (SSBool "c") st1 st2
                let st3u1 = mrgIf (SSBool "c") st1 st2
                StateStrict.runStateT st3 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateStrict.runStateT st3 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
                StateStrict.runStateT st31 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateStrict.runStateT st31 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
                StateStrict.runStateT st3u1 2 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"), 4)
                StateStrict.runStateT st3u1 3 @=? mrgIf (SSBool "c") (mrgSingle (SSBool "a", 5)) (mrgSingle (SSBool "b", 6))
            ],
          testCase "ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer)" $ do
            let c1 :: ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer) = ContT $ \f -> f (SSBool "a", 2)
            let c2 :: ContT (SBool, Integer) (UnionMBase SBool) (SBool, Integer) = ContT $ \f -> f (SSBool "b", 3)
            let c3 = gmrgIte (SSBool "c") c1 c2
            let c3u1 = mrgIf (SSBool "c") c1 c2
            let r =
                  mrgIf
                    (SSBool "c")
                    (mrgIf (SSBool "p") (mrgSingle (SSBool "a", 2)) (mrgSingle (Not $ SSBool "a", 3)))
                    (mrgIf (SSBool "p") (mrgSingle (SSBool "b", 3)) (mrgSingle (Not $ SSBool "b", 4)))
            let f (a, x) = mrgIf (SSBool "p") (mrgSingle (a, x)) (mrgSingle (nots a, x + 1))
            runContT c3 f @=? r
            runContT c3u1 f @=? r,
          testGroup
            "RWST (Integer, SBool) (Monoid.Sum Integer, AndMonoidSBool) (Integer, SBool) (UnionMBase SBool) (Integer, SBool)"
            [ testCase "Lazy" $ do
                let rws1 ::
                      RWSTLazy.RWST
                        (Integer, SBool)
                        (Monoid.Sum Integer, AndMonoidSBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br &&~ bs), (ir - is, br ||~ bs), (Monoid.Sum $ ir * is, AndMonoidSBool $ bs &&~ br))
                let rws2 ::
                      RWSTLazy.RWST
                        (Integer, SBool)
                        (Monoid.Sum Integer, AndMonoidSBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTLazy.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br ||~ bs), (ir - is, br &&~ bs), (Monoid.Sum $ ir * is, AndMonoidSBool $ bs ||~ br))
                let rws3 = gmrgIte (SSBool "c") rws1 rws2
                let rws3u1 = mrgIf (SSBool "c") rws1 rws2

                let res1 :: UnionMBase SBool ((Integer, SBool), (Integer, SBool), (Monoid.Sum Integer, AndMonoidSBool)) =
                      mrgIf
                        (SSBool "c")
                        (mrgSingle ((1, And (SSBool "a") (SSBool "b")), (-1, Or (SSBool "a") (SSBool "b")), (0, AndMonoidSBool $ And (SSBool "b") (SSBool "a"))))
                        (mrgSingle ((1, Or (SSBool "a") (SSBool "b")), (-1, And (SSBool "a") (SSBool "b")), (0, AndMonoidSBool $ Or (SSBool "b") (SSBool "a"))))
                RWSTLazy.runRWST rws3 (0, SSBool "a") (1, SSBool "b") @=? res1
                RWSTLazy.runRWST rws3u1 (0, SSBool "a") (1, SSBool "b") @=? res1,
              testCase "Strict" $ do
                let rws1 ::
                      RWSTStrict.RWST
                        (Integer, SBool)
                        (Monoid.Sum Integer, AndMonoidSBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br &&~ bs), (ir - is, br ||~ bs), (Monoid.Sum $ ir * is, AndMonoidSBool $ bs &&~ br))
                let rws2 ::
                      RWSTStrict.RWST
                        (Integer, SBool)
                        (Monoid.Sum Integer, AndMonoidSBool)
                        (Integer, SBool)
                        (UnionMBase SBool)
                        (Integer, SBool) =
                        RWSTStrict.RWST $ \(ir, br) (is, bs) ->
                          mrgSingle ((ir + is, br ||~ bs), (ir - is, br &&~ bs), (Monoid.Sum $ ir * is, AndMonoidSBool $ bs ||~ br))
                let rws3 = gmrgIte (SSBool "c") rws1 rws2
                let rws3u1 = mrgIf (SSBool "c") rws1 rws2

                let res1 :: UnionMBase SBool ((Integer, SBool), (Integer, SBool), (Monoid.Sum Integer, AndMonoidSBool)) =
                      mrgIf
                        (SSBool "c")
                        (mrgSingle ((1, And (SSBool "a") (SSBool "b")), (-1, Or (SSBool "a") (SSBool "b")), (0, AndMonoidSBool $ And (SSBool "b") (SSBool "a"))))
                        (mrgSingle ((1, Or (SSBool "a") (SSBool "b")), (-1, And (SSBool "a") (SSBool "b")), (0, AndMonoidSBool $ Or (SSBool "b") (SSBool "a"))))
                RWSTStrict.runRWST rws3 (0, SSBool "a") (1, SSBool "b") @=? res1
                RWSTStrict.runRWST rws3u1 (0, SSBool "a") (1, SSBool "b") @=? res1
            ],
          testGroup
            "WriterT (Monoid.Sum Integer) (UnionMBase SBool) SBool"
            [ testCase "Lazy" $ do
                let st1 :: WriterLazy.WriterT (Monoid.Sum Integer) (UnionMBase SBool) SBool =
                      WriterLazy.WriterT $ mrgSingle (SSBool "a", 1)
                let st2 :: WriterLazy.WriterT (Monoid.Sum Integer) (UnionMBase SBool) SBool =
                      WriterLazy.WriterT $ mrgSingle (SSBool "b", 2)
                let st3 :: WriterLazy.WriterT (Monoid.Sum Integer) (UnionMBase SBool) SBool =
                      WriterLazy.WriterT $ mrgSingle (SSBool "c", 1)
                let st4 = gmrgIte (SSBool "d") st1 st2
                let st41 = gmrgIte1 (SSBool "d") st1 st2
                let st4u1 = mrgIf (SSBool "d") st1 st2
                let st5 = gmrgIte (SSBool "d") st1 st3
                let st51 = gmrgIte1 (SSBool "d") st1 st3
                let st5u1 = mrgIf (SSBool "d") st1 st3
                WriterLazy.runWriterT st4 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterLazy.runWriterT st41 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterLazy.runWriterT st4u1 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterLazy.runWriterT st5 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
                WriterLazy.runWriterT st51 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
                WriterLazy.runWriterT st5u1 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1),
              testCase "Strict" $ do
                let st1 :: WriterStrict.WriterT (Monoid.Sum Integer) (UnionMBase SBool) SBool =
                      WriterStrict.WriterT $ mrgSingle (SSBool "a", 1)
                let st2 :: WriterStrict.WriterT (Monoid.Sum Integer) (UnionMBase SBool) SBool =
                      WriterStrict.WriterT $ mrgSingle (SSBool "b", 2)
                let st3 :: WriterStrict.WriterT (Monoid.Sum Integer) (UnionMBase SBool) SBool =
                      WriterStrict.WriterT $ mrgSingle (SSBool "c", 1)
                let st4 = gmrgIte (SSBool "d") st1 st2
                let st41 = gmrgIte1 (SSBool "d") st1 st2
                let st4u1 = mrgIf (SSBool "d") st1 st2
                let st5 = gmrgIte (SSBool "d") st1 st3
                let st51 = gmrgIte1 (SSBool "d") st1 st3
                let st5u1 = mrgIf (SSBool "d") st1 st3
                WriterStrict.runWriterT st4 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterStrict.runWriterT st41 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterStrict.runWriterT st4u1 @=? mrgIf (SSBool "d") (mrgSingle (SSBool "a", 1)) (mrgSingle (SSBool "b", 2))
                WriterStrict.runWriterT st5 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
                WriterStrict.runWriterT st51 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
                WriterStrict.runWriterT st5u1 @=? mrgSingle (ITE (SSBool "d") (SSBool "a") (SSBool "c"), 1)
            ],
          testCase "ReaderT Integer (UnionMBase SBool) Integer" $ do
            let r1 :: ReaderT Integer (UnionMBase SBool) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x + 2
            let r2 :: ReaderT Integer (UnionMBase SBool) Integer =
                  ReaderT $ \(x :: Integer) -> mrgSingle $ x * 2
            let r3 = gmrgIte (SSBool "c") r1 r2
            -- let r31 = gmrgIte1 (SSBool "c") r1 r2
            let r3u1 = mrgIf (SSBool "c") r1 r2
            runReaderT r3 2 @=? mrgSingle 4
            runReaderT r3 3 @=? mrgIf (SSBool "c") (mrgSingle 5) (mrgSingle 6)
            -- runReaderT r31 2 @=? mrgSingle 4
            -- runReaderT r31 3 @=? mrgIf (SSBool "c") (mrgSingle 5) (mrgSingle 6)
            runReaderT r3u1 2 @=? mrgSingle 4
            runReaderT r3u1 3 @=? mrgIf (SSBool "c") (mrgSingle 5) (mrgSingle 6)

            let r4 :: ReaderT SBool (UnionMBase SBool) SBool =
                  ReaderT $ \x -> mrgSingle $ x &&~ SSBool "x"
            let r5 :: ReaderT SBool (UnionMBase SBool) SBool =
                  ReaderT $ \x -> mrgSingle $ x ||~ SSBool "y"
            let r61 = gmrgIte1 (SSBool "c") r4 r5
            runReaderT r61 (SSBool "a") @=? mrgSingle (ites (SSBool "c") (SSBool "a" &&~ SSBool "x") (SSBool "a" ||~ SSBool "y")),
          testCase "Identity SBool" $ do
            let i1 :: Identity SBool = Identity $ SSBool "a"
            let i2 :: Identity SBool = Identity $ SSBool "b"
            let i3 = gmrgIte (SSBool "c") i1 i2
            let i31 = gmrgIte1 (SSBool "c") i1 i2
            runIdentity i3 @=? ITE (SSBool "c") (SSBool "a") (SSBool "b")
            runIdentity i31 @=? ITE (SSBool "c") (SSBool "a") (SSBool "b"),
          testCase "IdentityT (UnionMBase SBool) SBool" $ do
            let i1 :: IdentityT (UnionMBase SBool) SBool = IdentityT $ mrgSingle $ SSBool "a"
            let i2 :: IdentityT (UnionMBase SBool) SBool = IdentityT $ mrgSingle $ SSBool "b"
            let i3 = gmrgIte (SSBool "c") i1 i2
            let i31 = gmrgIte1 (SSBool "c") i1 i2
            let i3u1 = mrgIf (SSBool "c") i1 i2
            runIdentityT i3 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runIdentityT i31 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"))
            runIdentityT i3u1 @=? mrgSingle (ITE (SSBool "c") (SSBool "a") (SSBool "b"))
        ]
    ]
