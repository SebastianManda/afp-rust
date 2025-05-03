module InterpTests where

import Test.Hspec

import Run ( run )
import Value

import Data.Either ( isLeft )

interpTest :: String -> Value -> Spec
interpTest input expected =
    it (input ++ " should be " ++ show expected) $ do
            run input `shouldBe` Right expected

tcErrorTest :: String -> Spec
tcErrorTest input =
    it (input ++ " should not type check") $ do
            run input `shouldSatisfy` isLeft

test :: IO ()
test = hspec $ do
    describe "Interpreter: good weather tests" $ do
        interpTest "True && False" (VBool False)
        interpTest "let x = 3; x * x" (VInt 9)
        interpTest "fun square (x : int) = x * x; square 3" (VInt 9)

    describe "Interpreter: order of operation" $ do
        it "multiplication should come before addition" $ do
            let result = Right (VInt 14)
            run "2 + 3 * 4" `shouldBe` result
            run "4 * 3 + 2" `shouldBe` result

        it "brackets should have the highest priority" $ do
            run "(2 + 3) * 4" `shouldBe` Right (VInt 20)

    describe "Interpreter: mutation" $ do
        it "should allow mutation of variables" $ do
            run "let mut x = 2; set x = x + 1; x" `shouldBe` Right (VInt 3)
            run "let mut x = 2; set x = 3; set x = 4; x + 1" `shouldBe` Right (VInt 5)

        it "should not allow mutation of immutable variables" $ do
            run "let x = 2; set x = 1; x" `shouldSatisfy` isLeft
            run "let x = 2; set x = 3; x + 1" `shouldSatisfy` isLeft