module Value where

import Lang.Abs ( Exp
                , Ident
                , Type, Stmt, Param )
import Env
import Evaluator (Result)

data Value 
    = VInt Integer
    | VBool Bool
    | VEmpty
  deriving (Show, Eq)

data Closure = Fun [Param] (InterpEnv -> Result (Either Value InterpEnv))
  -- deriving (Show, Eq)

data TClosure = TFun [Param] Type (TypeCheckEnv -> Result (Either Type TypeCheckEnv))
  -- deriving (Show, Eq)

type InterpEnv = (Env Value, Env Closure)
type TypeCheckEnv = (Env Type, Env TClosure)
