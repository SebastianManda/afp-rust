module TypeCheckTests where

import Test.Hspec

import Run ( infertype )
import Lang.Abs ( Type(..) )

import Data.Either ( isLeft )

tcTest :: String -> Type -> Spec
tcTest input expected =
    it (input ++ " should type check to " ++ show expected) $ do
            infertype input `shouldBe` Right expected

tcErrorTest :: String -> Spec
tcErrorTest input =
    it (input ++ " should not type check") $ do
            infertype input `shouldSatisfy` isLeft

test :: IO ()
test = hspec $ do
    describe "typeChecker: good weather tests" $ do
        tcTest "True && False" TBool
        tcTest "let x = 3; x * x" TInt
        tcTest "fun isZero (x : int) = x == 0; isZero 42" TBool
        tcTest "fun square (x : int) = x * x;" TVoid

    describe "typeChecker: bad weather tests" $ do
        tcErrorTest "let x = 2 in x + True"
        tcErrorTest "True && 3"
        tcErrorTest "fun increment (x : int) = x + 1; increment True"

    describe "typeChecker: mutation" $ do
        tcTest "let mut x = 2; set x = 1; x" TInt
        tcTest "let mut x = 2; set x = True; x" TBool
        tcErrorTest "let x = 2; set x = True; x"

    describe "typeChecker: control-flow" $ do
        tcTest "let mut x = 2; if x == 2 { set x = x + 1; } else { set x = x - 1; } x" TInt
        tcTest "let mut x = 2; if x == 3 { set x = x + 1; } else { set x = x - 1; } x" TInt
        tcTest "let mut x = 2; if x == 3 { True } else { False }" TBool
        tcTest "let mut x = 2; while x < 5 { set x = x + 1; } x" TInt
        tcTest "let mut x = 6; while x < 5 { set x = x + 1; } x" TInt
        tcErrorTest "let mut x = 2; if x == 3 { True } else { 0 }"