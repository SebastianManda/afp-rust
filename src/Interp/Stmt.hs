module Interp.Stmt where

import qualified Data.Map as Map
import qualified Interp.Expr as E (interp, appFunc)

import Evaluator (Result, throw)
import Control.Monad (foldM)
import Lang.Abs ( Stmt(..), Exp, Ident, Param(..) )
import Value ( Value( VBool, VVoid ), Closure( Fun ), InterpEnv )
import Env ( Env, bind, bindMut, mergeDEnv )
import Interp.Env ( find, update )
import Shared ( collapseEnvs )

-- | Interprets a list of statements. Returns the final environment.
prepare :: [Stmt] -> InterpEnv -> Result (Either Value InterpEnv)
prepare []           env = return (Right env)
prepare (stmt:stmts) env = do
    case interp stmt env of
        Left err           -> Left err
        Right (Left val)   -> return (Left val)
        Right (Right nenv) -> do prepare stmts nenv

-- | Propagates the value of the expression through the statements. (Helper function for control flow)
propagate :: [Stmt] -> Exp -> InterpEnv -> Result (Either Value InterpEnv)
propagate stmts e env = do
    nenv <- prepare stmts env
    case nenv of
        Left v -> return (Left v)
        Right nenv' -> do
            val <- E.interp e nenv'
            case val of
                VVoid -> return (Right (collapseEnvs env nenv' mergeDEnv))
                _      -> return (Left val)

-- STATEMENT INTERPRETER -------------------------------------------------------------

interp :: Stmt -> InterpEnv -> Result (Either Value InterpEnv)

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
interp (SFun id params _ body expr) env@(vars, funs) = do
    let fenv = propagate body expr
    let fun = Fun params fenv
    return (Right (vars, bind id fun funs))

interp (SApp id args) env = E.appFunc id args env

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
