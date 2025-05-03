module TypeCheck.Stmt where

import Evaluator

import Env

import Value ( TClosure( TFun ) )

import Lang.Abs ( Stmt(..)
                , Type
                , Type( TBool, TVoid )
                , Exp )

import qualified TypeCheck.Expr as E
import Control.Monad (foldM)
import Data.List (nub)

-- | Prepares a list of statements. Returns the final environment.
prepare :: [Stmt] -> (Env Type, Env TClosure) -> Result (Either Type (Env Type, Env TClosure))
prepare []           env = return (Right env)
prepare (stmt:stmts) env = do
    case infer stmt env of
        Left err           -> Left err
        Right (Left val)   -> return (Left val)
        Right (Right nenv) -> do prepare stmts nenv

-- | Propagates the type of the expression through the statements. (Helper function for control flow)
propagate :: [Stmt] -> Exp -> (Env Type, Env TClosure) -> Result (Either Type (Env Type, Env TClosure))
propagate stmts t env = do
    nenv <- prepare stmts env
    case nenv of
        Left v -> return (Left v)
        Right nenv' -> do
            val <- E.infer t nenv'
            case val of
                TVoid -> return (Right nenv')
                _      -> return (Left val)

-- STATEMENT TYPE CHECKER ------------------------------------------------------------

infer :: Stmt -> (Env Type, Env TClosure) -> Result (Either Type (Env Type, Env TClosure))

-- Variables
infer (SLet x e) env@(vars, funs) = do
    t <- E.infer e env
    return (Right (bind x t vars, funs))

infer (SMut x e) env@(vars, funs) = do
    t <- E.infer e env
    return (Right (bindMut x t vars, funs))

infer (SSet x e) env@(vars, funs) = do
    t <- E.infer e env
    case update x t vars of
        Nothing     -> throw "Cannot update immutable variable"
        Just nvars  -> return (Right (nvars, funs))

-- Functions
infer (SFun f x t e) (vars, funs) = do
    ret <- E.infer e (bind x t vars, funs)
    return (Right (vars, bind f (TFun t ret) funs))

-- Control Flow
infer (SIf c st t) env = do
    cond <- E.infer c env
    case cond of
        TBool -> propagate st t env
        _     -> throw "If condition must be a boolean"

infer (SIfElse c st t se e) env = do
    cond <- E.infer c env
    case cond of
        TBool -> do
            envt <- propagate st t env
            enve <- propagate se e env
            case (envt, enve) of
                (Left t1, Left t2) | t1 == t2 -> return (Left t1)
                (Right _, Right _)            -> return (Right env)
                _                             -> throw "Both branches of an if must have the same type"
        _     -> throw "If condition must be a boolean"

infer (SWhile c st) env = do
    cond <- E.infer c env
    case cond of
        TBool -> return (Right env)
        _     -> throw "While condition must be a boolean"

