module TypeCheck.Expr where

import Evaluator ( Result, throw )
import Env ( Env, find, bindParams, empty )
import Value ( TClosure( TFun ), TypeCheckEnv )
import Lang.Abs ( Exp(..), Ident, Type(..), Param(..) )
import Shared ( verifyArgs, checkArgs )

arithmetic :: (Exp, Exp) -> TypeCheckEnv -> Result Type
arithmetic (e1, e2) env = do
    t1 <- infer e1 env
    t2 <- infer e2 env
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        _            -> throw "Arithmetic can only be performed on integers"

logic :: (Exp, Exp) -> TypeCheckEnv -> Result Type
logic (e1, e2) env =  do
    t1 <- infer e1 env
    t2 <- infer e2 env
    case (t1, t2) of
        (TBool, TBool) -> return TBool
        _              -> throw "Boolean operations can only be performed on booleans"

comparison :: (Exp, Exp) -> TypeCheckEnv -> Result Type
comparison (e1, e2) env =  do
    t1 <- infer e1 env
    t2 <- infer e2 env
    case (t1, t2) of
        (TInt, TInt) -> return TBool
        (t1,   t2  ) -> throw $ "Cannot compare " ++ show t1 ++ " with " ++ show t2

-- EXPRESSION TYPE CHECKER -----------------------------------------------------------

infer :: Exp -> TypeCheckEnv -> Result Type

-- Void
infer EVoid _ = return TVoid

-- Arithmetic
infer (EInt _) _ = return TInt

infer (EMul e1 e2) env = arithmetic (e1, e2) env
infer (EDiv e1 e2) env = arithmetic (e1, e2) env
infer (EAdd e1 e2) env = arithmetic (e1, e2) env
infer (ESub e1 e2) env = arithmetic (e1, e2) env

-- Booleans
infer ETrue  _ = return TBool
infer EFalse _ = return TBool

infer (ENot e) env = do
    t <- infer e env
    case t of
        TBool -> return TBool
        _     -> throw "Boolean operations can only be performed on booleans"
infer (EAnd e1 e2) env = logic (e1, e2) env
infer (EOr  e1 e2) env = logic (e1, e2) env

-- Comparisons
infer (EEq e1 e2) env = do
    t1 <- infer e1 env
    t2 <- infer e2 env
    if t1 == t2 then return TBool
    else throw "Cannot compare different types"
infer (ELt  e1 e2) env = comparison (e1, e2) env
infer (EGt  e1 e2) env = comparison (e1, e2) env
infer (ELeq e1 e2) env = comparison (e1, e2) env
infer (EGeq e1 e2) env = comparison (e1, e2) env

-- Control flow
infer (EIf c iff els) env = do
    cond <- infer c env
    case cond of
        TBool -> do
            tI <- infer iff env
            tE <- infer els env
            case (tI, tE) of
                (tI, tE) | tI == tE -> return tI
                _                   -> throw "Both branches of an if must have the same type"
        _     -> throw "Condition must be a boolean"

-- Let bindings
infer (EVar x) (vars, _) =
    case find x vars of
        Just t  -> return t
        Nothing -> throw $ "Variable " ++ show x ++ " is not bound"

-- Functions
infer (EApp id args) env = case appFunc id args env of 
    Left err     -> throw err
    Right result -> case result of
        Left t     -> return t
        Right nenv -> return TVoid

-- | Common function for function application. Binds the arguments to their values and prepares the function call.
appFunc :: Ident -> [Exp] -> TypeCheckEnv -> Result (Either Type TypeCheckEnv)
appFunc id args env@(vars, funs) = do
    case find id funs of
        Nothing                     -> throw "Arguments can only be applied to functions"
        Just (TFun params rtn fenv) -> do
            let paramTypes = getParamTypes params
            case verifyArgs (map (`infer` env) args) of
                Left err       -> throw err
                Right argTypes -> if not (checkArgs argTypes paramTypes)
                    then throw "Function argument type mismatch"
                    else do
                        let fvars = bindParams (zip params argTypes) empty
                        case fenv (fvars, empty) of
                            Left err     -> throw err
                            Right result -> case result of
                                Left t     -> if t == rtn 
                                    then return (Left rtn)
                                    else throw "Function return type mismatch"
                                Right nenv -> if rtn == TVoid 
                                    then return (Right env)
                                    else throw "Function return type mismatch"

getParamTypes :: [Param] -> [Type]
getParamTypes [] = []
getParamTypes (PVal _ t:ps) = t : getParamTypes ps
getParamTypes (PMut _ t:ps) = t : getParamTypes ps


