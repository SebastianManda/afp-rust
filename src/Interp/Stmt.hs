module Interp.Stmt where

import Evaluator

import Lang.Abs ( Stmt(..), Exp, Ident )

import Env
import Value ( Value( VBool, VEmpty )
             , Closure( Fun ) )

import qualified Interp.Expr as E
import Control.Monad (foldM)
import Data.Map (toList)
import qualified Data.Map as Map

-- | Interprets a list of statements. Returns the final environment.
prepare :: [Stmt] -> (Env Value, Env Closure) -> Result (Either Value (Env Value, Env Closure))
prepare []           env = return (Right env)
prepare (stmt:stmts) env = do
    case interp stmt env of
        Left err           -> Left err
        Right (Left val)   -> return (Left val)
        Right (Right nenv) -> do prepare stmts nenv

-- | Propagates the value of the expression through the statements. (Helper function for control flow)
propagate :: [Stmt] -> Exp -> (Env Value, Env Closure) -> Result (Either Value (Env Value, Env Closure))
propagate stmts e env = do
    nenv <- prepare stmts env
    case nenv of
        Left v -> return (Left v)
        Right nenv' -> do
            val <- E.interp e nenv'
            case val of
                VEmpty -> return (Right (collapseEnvs env nenv'))
                _      -> return (Left val)

-- | Collapses the environments by merging the variables and functions. Updates original env with changed mutables.
collapseEnvs :: (Env Value, Env Closure) -> (Env Value, Env Closure) -> (Env Value, Env Closure)
collapseEnvs (vars1, funs1) (vars2, funs2) = (vars1 `merge` toList vars2, funs1 `merge` toList funs2)
  where
    merge :: Env a -> [(Ident, (a, Bool))] -> Env a
    merge oenv nenv = case nenv of
        [] -> oenv
        ((k, (v, b)):kvs) -> case Map.lookup k oenv of
            Just (_, True) | b -> case update k v oenv of
                Just noenv -> merge noenv kvs
                Nothing    -> merge oenv kvs
            _ -> merge oenv kvs

-- STATEMENT INTERPRETER -------------------------------------------------------------

interp :: Stmt -> (Env Value, Env Closure) -> Result (Either Value (Env Value, Env Closure))

-- Variables
interp (SLet x e) env@(vars, funs) = do
    val <- E.interp e env
    return (Right (bind x val vars, funs))

interp (SMut x e) env@(vars, funs) = do
    val <- E.interp e env
    return (Right (bindMut x val vars, funs))

interp (SSet x e) env@(vars, funs) = do
    val <- E.interp e env
    case update x val vars of
        Nothing    -> throw "Cannot update immutable variable"
        Just nvars -> return (Right (nvars, funs))

-- Functions
interp (SFun f x _ e) env@(vars, funs) = return (Right (vars, bind f (Fun x e) funs))

-- Control Flow
interp (SIf c st t) env = do
    cond <- E.interp c env
    case cond of
        VBool True  -> propagate st t env
        VBool False -> return (Right env)
        _           -> throw "If condition must be a boolean"

interp (SIfElse c st t se e) env = do
    cond <- E.interp c env
    case cond of
        VBool True  -> propagate st t env
        VBool False -> propagate se e env
        _           -> throw "If condition must be a boolean"

interp (SWhile c st) env = do
    cond <- E.interp c env
    case cond of
        VBool True  -> do
            nenv <- prepare st env
            case nenv of
                Left v      -> return (Left v)
                Right nenv' -> interp (SWhile c st) nenv'
        VBool False -> return (Right env)
        _           -> throw "While condition must be a boolean"