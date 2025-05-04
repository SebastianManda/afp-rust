module Main where

import qualified TypeCheckTests as TypeCheck ( test )
import qualified InterpTests as Interp ( test )
import qualified BogusTests as Bogus ( test )
import qualified ParseTests as Parse ( test )

main :: IO ()
main = do
    Parse.test
    TypeCheck.test
    Interp.test
    Bogus.test

