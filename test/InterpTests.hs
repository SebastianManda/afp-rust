module InterpTests where

import Test.Hspec

import Run ( run )
import Value

import Data.Either ( isLeft )
import Value (Value(VVoid))

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
        -- interpTest "fun square (x : int) = x * x; square 3" (VInt 9)

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

    describe "Interpreter: control-flow" $ do
        it "should work for if-then " $ do
            run "let mut x = 2; if x == 2 { set x = x + 1; } x + 1" `shouldBe` Right (VInt 4)
            run "let mut x = 2; if x == 2 { x + 2 }" `shouldBe` Right (VInt 4)
        it "should work for if-then-else" $ do
            run "let mut x = 2; if x == 2 { set x = x + 1; } else { set x = x - 1; } x" `shouldBe` Right (VInt 3)
            run "let mut x = 2; if x == 3 { set x = x + 1; } else { set x = x - 1; } x" `shouldBe` Right (VInt 1)
            run "let mut x = 2; if x == 3 { x + 1 } else { x - 1 }" `shouldBe` Right (VInt 1)
        it "should work for while" $ do
            run "let mut x = 2; while x < 5 { set x = x + 1; } x" `shouldBe` Right (VInt 5)
            run "let mut x = 6; while x < 5 { set x = x + 1; } x" `shouldBe` Right (VInt 6)
            run "let mut x = 2; while x < 5 { set x = x + 1; }" `shouldBe` Right VVoid
        it "should not update env with new vars" $ do
            run "let mut x = 2; if x == 2 { let y = 3; } y" `shouldSatisfy` isLeft
            run "let mut x = 2; if x == 2 { let y = 3; } else { let y = 4; } y" `shouldSatisfy` isLeft
            run "let mut x = 2; while x < 5 { let y = 3; set x = x + 1; } x + 1" `shouldBe` Right (VInt 6)
            run "let mut x = 2; while x < 5 { let y = 3; set x = x + 1; } y" `shouldSatisfy` isLeft
        it "should not work for non-boolean conditions" $ do
            run "let mut x = 2; if x { set x = x + 1; } x + 1" `shouldSatisfy` isLeft
            run "let mut x = 2; if 32 { set x = x + 1; } else { set x = x - 1; } x" `shouldSatisfy` isLeft
            run "let mut x = 2; while x { set x = x + 1; }" `shouldSatisfy` isLeft

    describe "Interpreter: functions" $ do
        it "should work for empty args" $ do
            run "fun f() -> void = {} f()" `shouldBe` Right VVoid
            run "fun f() -> int = { 1 } f() + 1" `shouldBe` Right (VInt 2)
            run "fun f() -> int = { 1 } apply f();" `shouldBe` Right (VInt 1)
            run "fun f() -> int = { 1 } apply f();" `shouldBe` Right (VInt 1)
        it "should work for functions with arguments" $ do
            run "fun mult(x: int, y: int) -> void = {} mult(2, 3)" `shouldBe` Right VVoid
            run "fun mult(x: int, y: int) -> int = { x * y } mult(2, 3)" `shouldBe` Right (VInt 6)
            run "fun mult(x: int, y: int) -> int = { let z = 1; x * y } mult(2, 3) + 1" `shouldBe` Right (VInt 7)
            run "let x = 2; fun mult(x: int) -> void = {} apply mult(x); x" `shouldBe` Right (VInt 2)
            run "fun f(x: bool) -> int = { if x { 2 } else { 13 } } f(True)" `shouldBe` Right (VInt 2)
        it "should work for functions with mutable arguments" $ do
            run "fun f(mut x: int) -> void = { set x = 3; } let mut x = 1; apply f(x); x" `shouldBe` Right (VInt 3)
            run "fun f(mut y: int) -> void = { set y = y + 1; } let mut x = 1; apply f(x); apply f(x); x" `shouldBe` Right (VInt 3)
            run "fun f(mut x: int) -> void = { set x = x + 1; } let mut x = 1; apply f(x); apply f(x); x" `shouldBe` Right (VInt 3)
        it "should work on functions within functions" $ do
            run "fun f(mut x: int) -> void = { fun g(mut x: int) -> void = { set x = x + 1; } apply g(x); } let mut x = 1; apply f(x); apply f(x); x" `shouldBe` Right (VInt 3)
        it "should not update env with unmutable vars" $ do
            run "fun f(mut x: int) -> void = { set x = x + 1; } let x = 2; apply f(x); x" `shouldBe` Right (VInt 2)
        it "should not update non mutavble vars" $ do
            run "fun f(x: int) -> void = { set x = x + 1; } let mut x = 2; apply f(x); x" `shouldSatisfy` isLeft