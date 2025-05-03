module Interp.Prog where

import Evaluator

import Env
import Value ( Value
             , Closure )

import Lang.Abs ( Program( Program )
                , Stmt )

import qualified Interp.Stmt as S
import qualified Interp.Expr as E
import Interp.Stmt (prepare)

-- PROGRAM INTERPRETER ---------------------------------------------------------------

interp :: Evaluator Value Closure
interp (Program stmts exp) env = do
    nenv <- prepare stmts env
    case nenv of
        Left val    -> return val
        Right nenv' -> do E.interp exp nenv'
