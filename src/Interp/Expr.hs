module Interp.Expr where

import qualified Env as E ( Env, empty, find, mergeDEnv )
import qualified Interp.Env as IE ( find, bindParams, mergeSEnv )

import Evaluator ( Result, throw ) 
import Lang.Abs ( Exp(..), Ident, Param(..) )
import Value ( Value(..), Closure( Fun ), InterpEnv )
import Shared ( verifyArgs, collapseEnvs )

arithmetic :: (Exp, Exp) -> InterpEnv -> (Integer -> Integer -> Integer) -> Result Value
arithmetic (e1, e2) env f = do
    v1 <- interp e1 env
    v2 <- interp e2 env
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VInt (f i1 i2)
        _                  -> throw "Arithmetic can only be performed on integers"

logic :: (Exp, Exp) -> InterpEnv -> (Bool -> Bool -> Bool) -> Result Value
logic (e1, e2) env f =  do
    v1 <- interp e1 env
    v2 <- interp e2 env
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (f b1 b2)
        _                    -> throw "Boolean operations can only be performed on booleans"

-- EXPRESSION INTERPRETER ------------------------------------------------------------

interp :: Exp -> InterpEnv -> Result Value

-- Void
interp EVoid _ = return VVoid

-- Arithmetic
interp (EInt i) _ = return $ VInt i

interp (EMul e1 e2) env = arithmetic (e1, e2) env (*)
interp (EDiv e1 e2) env = arithmetic (e1, e2) env div
interp (EAdd e1 e2) env = arithmetic (e1, e2) env (+)
interp (ESub e1 e2) env = arithmetic (e1, e2) env (-)

-- Booleans
interp ETrue  _ = return $ VBool True
interp EFalse _ = return $ VBool False

interp (ENot e) env =  do
    v <- interp e env
    case v of
        VBool b -> return $ VBool (not b)
        _       -> throw "Boolean operations can only be performed on booleans"
interp (EAnd e1 e2) env = logic (e1, e2) env (&&)
interp (EOr e1 e2)  env = logic (e1, e2) env (||)

-- Comparisons
interp (EEq e1 e2) env = do
    v1 <- interp e1 env
    v2 <- interp e2 env
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool (b1 == b2)
        (VInt  i1, VInt  i2) -> return $ VBool (i1 == i2)
        _                    -> throw "Cannot compare different types"
interp (ELt e1 e2) env = do
    v1 <- interp e1 env
    v2 <- interp e2 env
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 < i2)
        _                  -> throw "Cannot compare different types"
interp (EGt e1 e2) env = do
    v1 <- interp e1 env
    v2 <- interp e2 env
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 > i2)
        _                  -> throw "Cannot compare different types"
interp (ELeq e1 e2) env = do
    v1 <- interp e1 env
    v2 <- interp e2 env
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 <= i2)
        _                  -> throw "Cannot compare different types"
interp (EGeq e1 e2) env = do
    v1 <- interp e1 env
    v2 <- interp e2 env
    case (v1, v2) of
        (VInt i1, VInt i2) -> return $ VBool (i1 >= i2)
        _                  -> throw "Cannot compare different types"

-- Control flow
interp (EIf c iff els) env = do
    cond <- interp c env
    case cond of
        VBool True  -> interp iff env
        VBool False -> interp els env
        _           -> throw "Condition must be a boolean"

-- Functions
interp (EVar x) (vars, _) = case IE.find x vars of
    Just val -> return val
    Nothing  -> throw $ "Variable " ++ show x ++ " is not bound"

interp (EApp id args) env@(vars, funs) = case appFunc id args env of
    Left err     -> throw err
    Right result -> case result of
        Left val   -> return val
        Right nenv -> return VVoid

-- | Common function for function application. Binds the arguments to their values and prepares the function call.
appFunc :: Ident -> [Exp] -> InterpEnv -> Result (Either Value InterpEnv)
appFunc id args env@(vars, funs) = do
    case E.find id funs of
        Nothing                -> throw "Arguments can only be applied to functions"
        Just (Fun params fenv) -> do
            case verifyArgs (map (`interpForFun` env) args) of
                Left err      -> throw err
                Right argVals -> do
                    let fvars = IE.bindParams (zip params argVals) E.empty
                    case fenv (fvars, E.empty) of
                        Left err     -> throw err
                        Right (Left val) -> return (Left val)
                        Right (Right nenv) -> return (Right (collapseEnvs env nenv IE.mergeSEnv))

-- | Interpret a function variable with the given environment. Returns VVars instead of other Values if Identifiers are found.
interpForFun :: Exp -> InterpEnv -> Result Value 
interpForFun (EVar x) (vars, _) = do
    case IE.find x vars of
        Just val -> return (VVar x val)
        Nothing  -> throw $ "Variable " ++ show x ++ " is not bound"
interpForFun (EIf c t e) env = do
    cond <- interp c env
    case cond of
        VBool True  -> interpForFun t env
        VBool False -> interpForFun e env
        _           -> throw "Condition must be a boolean"
interpForFun exp env = interp exp env
