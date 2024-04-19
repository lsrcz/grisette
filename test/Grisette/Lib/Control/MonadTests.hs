{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Control.MonadTests (monadFunctionTests) where

import Control.Monad (MonadPlus (mzero), when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Grisette
  ( ITEOp (symIte),
    LogicalOp ((.&&), (.||)),
    SEq ((./=), (.==)),
    SOrd ((.<=)),
    Solvable (con),
    SymInteger,
    UnionM,
    UnionMergeable1 (mrgIfPropagatedStrategy),
    mrgFilterM,
    mrgGuard,
    mrgIf,
    mrgLiftM,
    mrgLiftM5,
    mrgMapAndUnzipM,
    mrgReplicateM,
    mrgReplicateM_,
    mrgSingle,
    mrgWhen,
    mrgZipWithM,
    symReplicateM_,
    symWhen,
  )
import Grisette.Lib.Control.Monad
  ( mrgAp,
    mrgFail,
    mrgFoldM,
    mrgFoldM_,
    mrgForever,
    mrgJoin,
    mrgLiftM2,
    mrgLiftM3,
    mrgLiftM4,
    mrgMfilter,
    mrgMplus,
    mrgMzero,
    mrgReturn,
    mrgUnless,
    mrgZipWithM_,
    symFilterM,
    symGuard,
    symMfilter,
    symReplicateM,
    symUnless,
    (.<$!>),
    (.<=<),
    (.=<<),
    (.>=>),
    (.>>),
    (.>>=),
  )
import Grisette.Lib.Control.Monad.Except
  ( mrgThrowError,
  )
import Grisette.TestUtil.NoMerge
  ( NoMerge (NoMerge),
    noMergeNotMerged,
    oneNotMerged,
  )
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

monadFunctionTests :: Test
monadFunctionTests =
  testGroup
    "Monad"
    [ testCase "mrgReturn" $ (mrgReturn 1 :: UnionM Integer) @?= mrgSingle 1,
      testGroup
        ".>>="
        [ testCase "merge result" $ do
            let actual =
                  mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
                    .>>= (\x -> return $ x * x)
            actual @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge argument" $ do
            let actual =
                  mrgIfPropagatedStrategy "a" (return (1 :: Int)) (return 1)
                    .>>= const (return NoMerge)
            actual @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        ".>>"
        [ testCase "merge result" $ do
            let actual =
                  ( mrgIfPropagatedStrategy "a" (return $ -1) (return 1) ::
                      UnionM Integer
                  )
                    .>> mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
            let expected =
                  mrgIf "a" (mrgReturn $ -1) (mrgReturn 1) ::
                    UnionM Integer
            actual @?= expected,
          testCase "merge lhs" $ do
            let actual =
                  ( mrgIfPropagatedStrategy "a" (return 1) (return 1) ::
                      UnionM Integer
                  )
                    .>> return NoMerge
            let expected = mrgReturn NoMerge :: UnionM NoMerge
            actual @?= expected
        ],
      testCase "mrgFail" $ do
        let actual = mrgFail "a" :: MaybeT UnionM Int
        actual @?= MaybeT (mrgSingle Nothing),
      testCase "mrgMzero" $ do
        (mrgMzero :: MaybeT UnionM Integer) @?= MaybeT (mrgReturn Nothing),
      testGroup
        "mrgMplus"
        [ testCase "merge result" $ do
            let actual = (mzero `mrgMplus` return 1 :: MaybeT UnionM Integer)
            actual @?= mrgReturn 1,
          testCase "merge lhs" $ do
            let lhs =
                  MaybeT $
                    mrgIfPropagatedStrategy "a" (return Nothing) (return Nothing) ::
                    MaybeT UnionM NoMerge
            let rhs = return NoMerge
            lhs `mrgMplus` rhs @?= MaybeT (mrgReturn $ Just NoMerge)
        ],
      testGroup
        ".=<<"
        [ testCase "merge result" $ do
            let actual =
                  (\x -> return $ x * x)
                    .=<< mrgIfPropagatedStrategy "a" (return $ -1) (return 1)
            actual @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge argument" $ do
            let actual =
                  const (return NoMerge)
                    .=<< mrgIfPropagatedStrategy "a" (return (1 :: Int)) (return 1)
            actual @?= (mrgSingle NoMerge :: UnionM NoMerge)
        ],
      testGroup
        ".>=>"
        [ testCase "merge result" $ do
            let lhs =
                  const
                    ( mrgIfPropagatedStrategy
                        "a"
                        (return $ -1)
                        (return 1) ::
                        UnionM Integer
                    )
            let actual = lhs .>=> (\x -> return $ x * x)
            actual (0 :: Integer) @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge lhs result" $ do
            let lhs =
                  const
                    ( mrgIfPropagatedStrategy
                        "a"
                        (return 1)
                        (return 1) ::
                        UnionM Integer
                    )
            let actual = lhs .>=> const (return NoMerge)
            actual (0 :: Integer) @?= mrgSingle NoMerge
        ],
      testGroup
        ".<=<"
        [ testCase "merge result" $ do
            let rhs =
                  const
                    ( mrgIfPropagatedStrategy
                        "a"
                        (return $ -1)
                        (return 1) ::
                        UnionM Integer
                    )
            let actual = (\x -> return $ x * x) .<=< rhs
            actual (0 :: Integer) @?= (mrgSingle 1 :: UnionM Integer),
          testCase "merge rhs result" $ do
            let rhs =
                  const
                    ( mrgIfPropagatedStrategy
                        "a"
                        (return 1)
                        (return 1) ::
                        UnionM Integer
                    )
            let actual = const (return NoMerge) .<=< rhs
            actual (0 :: Integer) @?= mrgSingle NoMerge
        ],
      testCase "mrgForever" $ do
        let f :: StateT Int (ExceptT NoMerge UnionM) ()
            f = do
              i <- get
              when (i == 0) $ throwError NoMerge
              put (i - 1)
              lift . lift $
                mrgIfPropagatedStrategy "a" (return ()) (return ())
        let actual = mrgForever f :: StateT Int (ExceptT NoMerge UnionM) NoMerge
        runStateT actual 10 @?= ExceptT (mrgReturn $ Left NoMerge),
      testCase "mrgJoin" $
        mrgJoin (return $ return 1) @?= (mrgSingle 1 :: UnionM Integer),
      testCase "mrgMfilter" $ do
        let actual = mrgMfilter (const True) (return 1 :: MaybeT UnionM Int)
        actual @?= (mrgSingle 1),
      testCase "symMfilter" $ do
        let actual = symMfilter (.== 0) (return "a" :: MaybeT UnionM SymInteger)
        let expected =
              mrgIf ("a" .== (0 :: SymInteger)) (mrgReturn "a") mrgMzero
        actual @?= expected,
      testGroup
        "mrgFilterM"
        [ testCase "merge result" $ do
            let actual = mrgFilterM (return . odd) [1, 2, 3, 4]
            let expected = mrgReturn [1, 3] :: UnionM [Int]
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  mrgFilterM
                    (const $ mrgIfPropagatedStrategy "a" (return True) (return True))
                    [NoMerge, NoMerge]
            let expected = mrgReturn [NoMerge, NoMerge] :: UnionM [NoMerge]
            actual @?= expected
        ],
      testGroup
        "symFilterM"
        [ testCase "merge result" $ do
            let actual = symFilterM (return . con . odd) [1, 2, 3, 4]
            let expected = mrgReturn [1, 3] :: UnionM [Int]
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  symFilterM
                    ( const $
                        mrgIfPropagatedStrategy
                          "a"
                          (return $ con True)
                          (return $ con True)
                    )
                    [NoMerge, NoMerge]
            let expected = mrgReturn [NoMerge, NoMerge] :: UnionM [NoMerge]
            actual @?= expected,
          testCase "symbolic semantics" $ do
            let a = "a" :: SymInteger
            let b = "b" :: SymInteger
            let actual = symFilterM (return . (./= 0)) [a, b]
            let expected =
                  mrgIf (a .== 0 .&& b .== 0) (return []) $
                    mrgIf
                      (a .== 0 .|| b .== 0)
                      (return [symIte (a .== 0) b a])
                      (return [a, b]) ::
                    UnionM [SymInteger]
            actual @?= expected
            actual .@?= expected
        ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "mrgMapAndUnzipM" $ do
          let actual =
                mrgMapAndUnzipM
                  ( \x ->
                      mrgIfPropagatedStrategy
                        "a"
                        (return (x, x + 1))
                        (return (x, x + 1))
                  )
                  [1 .. 100] ::
                  UnionM ([Int], [Int])
          let expected = mrgReturn ([1 .. 100], [2 .. 101])
          actual @?= expected,
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "mrgZipWithM" $ do
          let actual =
                mrgZipWithM
                  ( \x y ->
                      mrgIfPropagatedStrategy
                        "a"
                        (return $ x + y)
                        (return $ x + y)
                  )
                  [1 .. 100]
                  [1 .. 100] ::
                  UnionM ([Int])
          let expected = mrgReturn ((* 2) <$> [1 .. 100])
          actual @?= expected,
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "mrgZipWithM_" $ do
          let actual =
                mrgZipWithM_
                  ( \x y ->
                      mrgIfPropagatedStrategy
                        "a"
                        (return $ x + y)
                        (return $ x + y)
                  )
                  [1 .. 100 :: Int]
                  [1 .. 100] ::
                  UnionM ()
          let expected = mrgReturn ()
          actual @?= expected,
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "mrgFoldM" $ do
          let actual =
                mrgFoldM
                  ( \acc v ->
                      mrgIfPropagatedStrategy
                        "a"
                        (return $ acc + v)
                        (return $ acc + v)
                  )
                  10
                  [1 .. 100] ::
                  UnionM Integer
          actual @?= mrgReturn 5060,
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "mrgFoldM_" $ do
          let actual =
                mrgFoldM_
                  ( \acc v ->
                      mrgIfPropagatedStrategy
                        "a"
                        (return $ acc + v)
                        (return $ acc + v)
                  )
                  10
                  [1 .. 100 :: Int] ::
                  UnionM ()
          actual @?= mrgReturn (),
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "mrgReplicateM" $ do
          let actual =
                mrgReplicateM
                  100
                  (mrgIfPropagatedStrategy "a" (return 1) (return 1)) ::
                  UnionM [Int]
          actual @?= mrgReturn [1 | _ <- [1 .. 100]],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "symReplicateM"
          [ testCase "merge result and intermediate" $ do
              let actual =
                    symReplicateM
                      200
                      (100 :: SymInteger)
                      (mrgIfPropagatedStrategy "a" (return 1) (return 1)) ::
                      UnionM [Int]
              actual @?= mrgReturn [1 | _ <- [1 .. 100]],
            testCase "symbolic semantics" $ do
              let a = "a" :: SymInteger
              let actual =
                    symReplicateM
                      2
                      a
                      (mrgIfPropagatedStrategy "a" (return 1) (return 1)) ::
                      UnionM [Int]
              let expected =
                    mrgIf
                      (a .<= 0)
                      (return [])
                      (mrgIf (a .== 1) (return [1]) (return [1, 1])) ::
                      UnionM [Int]
              actual .@?= expected
          ],
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testCase "mrgReplicateM_" $ do
          let actual = mrgReplicateM_ 100 noMergeNotMerged :: UnionM ()
          actual @?= mrgReturn (),
      plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
        testGroup
          "symReplicateM_"
          [ testCase "merge result and intermediate" $ do
              let actual =
                    symReplicateM_ 200 (100 :: SymInteger) noMergeNotMerged ::
                      UnionM ()
              actual @?= mrgReturn ()
          ],
      testCase "mrgGuard" $ do
        mrgGuard True @?= (mrgReturn () :: MaybeT UnionM ())
        mrgGuard False @?= (MaybeT $ mrgReturn Nothing :: MaybeT UnionM ()),
      testCase "symGuard" $ do
        let expected =
              MaybeT $
                mrgIf "a" (return $ Just ()) (return Nothing) ::
                MaybeT UnionM ()
        symGuard "a" @?= expected,
      testCase "mrgWhen" $ do
        mrgWhen True (throwError "a")
          @?= (mrgThrowError "a" :: ExceptT String UnionM ())
        mrgWhen False (throwError "a")
          @?= (mrgReturn () :: ExceptT String UnionM ()),
      testCase "symWhen" $ do
        let expected =
              mrgIf "a" (mrgThrowError "x") (return ()) ::
                ExceptT String UnionM ()
        symWhen "a" (throwError "x") @?= expected,
      testCase "mrgUnless" $ do
        mrgUnless False (throwError "a")
          @?= (mrgThrowError "a" :: ExceptT String UnionM ())
        mrgUnless True (throwError "a")
          @?= (mrgReturn () :: ExceptT String UnionM ()),
      testCase "symUnless" $ do
        let expected =
              mrgIf "a" (return ()) (mrgThrowError "x") ::
                ExceptT String UnionM ()
        symUnless "a" (throwError "x") @?= expected,
      testGroup
        "mrgLiftM"
        [ testCase "merge result" $ do
            let actual = mrgLiftM (const 1) noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual = mrgLiftM (const NoMerge) oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        "mrgLiftM2"
        [ testCase "merge result" $ do
            let actual =
                  mrgLiftM2 (const $ const 1) noMergeNotMerged noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  mrgLiftM2 (const $ const NoMerge) oneNotMerged oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        "mrgLiftM3"
        [ testCase "merge result" $ do
            let actual =
                  mrgLiftM3
                    (const $ const $ const 1)
                    noMergeNotMerged
                    noMergeNotMerged
                    noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  mrgLiftM3
                    (const $ const $ const NoMerge)
                    oneNotMerged
                    oneNotMerged
                    oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        "mrgLiftM4"
        [ testCase "merge result" $ do
            let actual =
                  mrgLiftM4
                    (const $ const $ const $ const 1)
                    noMergeNotMerged
                    noMergeNotMerged
                    noMergeNotMerged
                    noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  mrgLiftM4
                    (const $ const $ const $ const NoMerge)
                    oneNotMerged
                    oneNotMerged
                    oneNotMerged
                    oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        "mrgLiftM5"
        [ testCase "merge result" $ do
            let actual =
                  mrgLiftM5
                    (const $ const $ const $ const $ const 1)
                    noMergeNotMerged
                    noMergeNotMerged
                    noMergeNotMerged
                    noMergeNotMerged
                    noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual =
                  mrgLiftM5
                    (const $ const $ const $ const $ const NoMerge)
                    oneNotMerged
                    oneNotMerged
                    oneNotMerged
                    oneNotMerged
                    oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        "mrgAp"
        [ testCase "merge result" $ do
            let actual = mrgAp (return $ const 1) noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual = mrgAp (return $ const NoMerge) oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ],
      testGroup
        ".<$!>"
        [ testCase "merge result" $ do
            let actual = const 1 .<$!> noMergeNotMerged
            let expected = mrgReturn 1 :: UnionM Int
            actual @?= expected,
          testCase "merge argument" $ do
            let actual = const NoMerge .<$!> oneNotMerged
            let expected = mrgReturn NoMerge
            actual @?= expected
        ]
    ]
