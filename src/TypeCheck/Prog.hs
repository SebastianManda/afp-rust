module TypeCheck.Prog where

import Evaluator

import Env

import Value ( TClosure )

import Lang.Abs ( Program( Program )
                , Stmt
                , Type )

import qualified TypeCheck.Stmt as S
import qualified TypeCheck.Expr as E
import qualified Lang.ErrM as S
import TypeCheck.Stmt (prepare)

-- PROGRAM TYPE CHECKER --------------------------------------------------------------

infer :: Evaluator Type TClosure
infer (Program stmts exp) env = do
    nenv <- prepare stmts env
    case nenv of
        Left val    -> return val
        Right nenv' -> do E.infer exp nenv'
