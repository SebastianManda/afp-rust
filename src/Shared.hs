module Shared where

import qualified Data.Map as Map

import Lang.Abs ( Ident, Type(..) )
import Evaluator ( Result )
import Env ( Env, bind, find, EnvMerge )
import Data.Map ( toList )
import Value ( InterpEnv, Value )

-- | Transforms a list of results into a list of a. Returns an error if any result is an error.
verifyArgs :: [Result a] -> Either String [a]
verifyArgs [] = Right []
verifyArgs (x:xs) = case x of
    Left err -> Left err
    Right v  -> case verifyArgs xs of
        Left err -> Left err
        Right vs -> Right (v:vs)

-- | Checks if the argument types match the function types. Returns an error if they do not match.
checkArgs :: [Type] -> [Type] -> Bool 
checkArgs [] [] = True
checkArgs [] _  = False
checkArgs _ []  = False
checkArgs (x:xs) (y:ys) = if x == y 
    then checkArgs xs ys 
    else False

-- | Collapses the environments by merging the variables and functions. Updates original env with changed mutables.
collapseEnvs :: InterpEnv -> InterpEnv -> EnvMerge Value -> InterpEnv
collapseEnvs (vars1, funs1) (vars2, _) mergeFun = (vars1 `mergeFun` toList vars2, funs1)