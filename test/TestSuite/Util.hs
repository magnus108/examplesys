module TestSuite.Util
  ( fromAssertions,
    failsWith,
    satisfies,
    succeeds,
    equals,
    returnsSame,
  )
where

import Piece.App.Error (AppError)
import Piece.App.Monad (App, AppEnv, runAppAsIO)
import Piece.CakeSlayer.Error (ErrorWithSource (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

fromAssertions :: String -> [Assertion] -> [TestTree]
fromAssertions name =
  zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

succeeds :: (Show a) => App a -> AppEnv -> Assertion
succeeds = (`satisfies` const True)

satisfies :: (Show a) => App a -> (a -> Bool) -> AppEnv -> Assertion
satisfies app p env =
  runAppAsIO env app >>= \case
    Left e -> assertFailure $ "Expected 'Success' but got: " <> show e
    Right a -> a `shouldSatisfy` p

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
shouldSatisfy v p = assertBool ("predicate failed on: " ++ show v) (p v)

failsWith :: (Show a) => App a -> AppError -> AppEnv -> Assertion
failsWith app err env =
  runAppAsIO env app >>= \case
    Left ErrorWithSource {..} -> errorWithSourceType @?= err
    Right a ->
      assertFailure $
        "Expected 'Failure' with: " <> show err <> " but got: " <> show a

equals :: (Show a, Eq a) => App a -> a -> AppEnv -> Assertion
equals app v env =
  runAppAsIO env app >>= \case
    Right a -> a @?= v
    Left e -> assertFailure $ "Expected 'Success' but got: " <> show e

returnsSame :: (Show a, Eq a) => App a -> App a -> AppEnv -> Assertion
returnsSame app1 app2 env = do
  result1 <- runAppAsIO env app1
  result2 <- runAppAsIO env app2
  result1 @?= result2
