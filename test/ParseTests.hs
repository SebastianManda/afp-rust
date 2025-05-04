module ParseTests where
import Test.Hspec

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn, return
  , FilePath
  , getContents, readFile, Bool (True, False)
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Lang.Lex   ( Token, mkPosToken )
import Lang.Par   ( pProgram, myLexer )
import Lang.Print ( Print, printTree )

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> return ()
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

isNotExitFailure :: Err a -> Bool
isNotExitFailure (Left _) = True
isNotExitFailure _        = False

parseTest :: String -> Spec
parseTest input = 
    it (input ++ " should parse") $ do
        run 2 pProgram input

test :: IO ()
test = hspec $ do
    describe "parser: basic tests" $ do
        parseTest "let x = 2; x + 1"


