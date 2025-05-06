module Interp.Env where

import qualified Data.Map as Map
import Lang.Abs ( Ident, Param( PVal, PMut ) )
import Env ( Env, EnvMerge, bind, bindMut)
import Value ( Value(..) )

find :: Ident -> Env Value -> Maybe Value
find id env = case Map.lookup id env of
    Nothing             -> Nothing
    Just (val, _, _, _) -> case val of
        VVar x v -> Just v
        _        -> Just val

update :: Ident -> Value -> Env Value -> Maybe (Env Value)
update id val env = case Map.lookup id env of
    Just (vl, True, refid, ovr) -> case vl of
        VVar x v -> Just (Map.insert id (VVar x val, True, refid, ovr) env)
        _        -> Just (Map.insert id (val, True, refid, ovr) env)
    _ -> Nothing

bindParams :: [(Param, Value)] -> Env Value -> Env Value
bindParams xs env = foldl (\acc (x, v) -> bindP x v acc) env xs

bindP :: Param -> Value -> Env Value -> Env Value
bindP (PVal id _) val = case val of
    VVar x v -> Map.insert id (v, False, Nothing, False)
    _        -> Map.insert id (val, False, Nothing, False)
bindP (PMut id _) val = case val of
    VVar x v -> Map.insert id (v, True, Just x, False)
    _        -> Map.insert id (val, True, Nothing, False)

mergeSEnv :: EnvMerge Value
mergeSEnv oenv nenv = case nenv of
    [] -> oenv
    ((id, (val, ismut, refid, _)):kvs) -> case refid of
        Just refid' -> case Map.lookup refid' oenv of
            Just (_, True, _, _) | ismut -> case update refid' val oenv of
                Just noenv -> mergeSEnv noenv kvs
                Nothing    -> mergeSEnv oenv kvs
            _ -> mergeSEnv oenv kvs
        Nothing -> mergeSEnv oenv kvs