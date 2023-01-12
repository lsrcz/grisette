{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.TestUtils.Assertions
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.TestUtils.Assertions where

import qualified Control.Exception as E
import Control.Monad
import Data.CallStack
import Data.Typeable (Typeable)

-- Interfaces
-- ----------

-- | An assertion is simply an 'IO' action. Assertion failure is indicated
-- by throwing an exception, typically 'HUnitFailure'.
--
-- Instead of throwing the exception directly, you should use
-- functions like 'assertFailure' and 'assertBool'.
--
-- Test cases are composed of a sequence of one or more assertions.
type Assertion = IO ()

-- | Unconditionally signals that a failure has occurred.  All
-- other assertions can be expressed with the form:
--
-- @
--    if conditionIsMet
--        then return ()
--        else assertFailure msg
-- @
assertFailure ::
  HasCallStack =>
  -- | A message that is displayed with the assertion failure
  String ->
  IO a
assertFailure msg = E.throwIO (HUnitFailure location msg)
  where
    location :: Maybe SrcLoc
    location = case reverse callStack of
      (_, loc) : _ -> Just loc
      [] -> Nothing

-- Conditional Assertion Functions
-- -------------------------------

-- | Asserts that the specified condition holds.
assertBool ::
  HasCallStack =>
  -- | The message that is displayed if the assertion fails
  String ->
  -- | The condition
  Bool ->
  Assertion
assertBool msg b = unless b (assertFailure msg)

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
assertEqual ::
  (Eq a, Show a, HasCallStack) =>
  -- | The message prefix
  String ->
  -- | The expected value
  a ->
  -- | The actual value
  a ->
  Assertion
assertEqual preface expected actual =
  unless (actual == expected) (assertFailure msg)
  where
    msg =
      (if null preface then "" else preface ++ "\n")
        ++ "expected: "
        ++ show expected
        ++ "\n but got: "
        ++ show actual

infix 1 @?, @=?, @?=

-- | Asserts that the specified actual value is equal to the expected value
--   (with the expected value on the left-hand side).
(@=?) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The expected value
  a ->
  -- | The actual value
  a ->
  Assertion
expected @=? actual = assertEqual "" expected actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the actual value on the left-hand side).
(@?=) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  Assertion
actual @?= expected = assertEqual "" expected actual

-- | An infix and flipped version of 'assertBool'. E.g. instead of
--
-- >assertBool "Non-empty list" (null [1])
--
-- you can write
--
-- >null [1] @? "Non-empty list"
--
-- '@?' is also overloaded to accept @'IO' 'Bool'@ predicates, so instead
-- of
--
-- > do
-- >   e <- doesFileExist "test"
-- >   e @? "File does not exist"
--
-- you can write
--
-- > doesFileExist "test" @? "File does not exist"
(@?) ::
  (AssertionPredicable t, HasCallStack) =>
  -- | A value of which the asserted condition is predicated
  t ->
  -- | A message that is displayed if the assertion fails
  String ->
  Assertion
predi @? msg = assertionPredicate predi >>= assertBool msg

-- | An ad-hoc class used to overload the '@?' operator.
--
-- The only intended instances of this class are @'Bool'@ and @'IO' 'Bool'@.
--
-- You shouldn't need to interact with this class directly.
class AssertionPredicable t where
  assertionPredicate :: t -> IO Bool

instance AssertionPredicable Bool where
  assertionPredicate = return

instance (AssertionPredicable t) => AssertionPredicable (IO t) where
  assertionPredicate = (>>= assertionPredicate)

-- | Exception thrown by 'assertFailure' etc.
data HUnitFailure = HUnitFailure (Maybe SrcLoc) String
  deriving (Eq, Show, Typeable)

instance E.Exception HUnitFailure

prependLocation :: Maybe SrcLoc -> String -> String
prependLocation mbloc s =
  case mbloc of
    Nothing -> s
    Just loc -> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ":\n" ++ s
