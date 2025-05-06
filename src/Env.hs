module Env where

import qualified Data.Map as Map
import Lang.Abs ( Ident, Param( PVal, PMut ) )

-- | Env a = Map.Map Ident (a, isMutable, refIdentifier, isOveride)
type Env a = Map.Map Ident (EnvVar a)
type EnvVar a = (a, Bool, Maybe Ident, Bool)
type EnvMerge a = Env a -> [(Ident, EnvVar a)] -> Env a

empty :: Env a
empty = Map.empty

find :: Ident -> Env a -> Maybe a
find id env =  case Map.lookup id env of
    Just (v, _, _, _) -> Just v
    Nothing        -> Nothing

bind :: Ident -> a -> Env a -> Env a
bind id val env = case Map.lookup id env of
    Just _  -> Map.insert id (val, False, Nothing, True) env
    Nothing -> Map.insert id (val, False, Nothing, False) env

bindMut :: Ident -> a -> Env a -> Env a
bindMut id val env = case Map.lookup id env of
    Just _  -> Map.insert id (val, True, Nothing, True) env
    Nothing -> Map.insert id (val, True, Nothing, False) env

bindP :: Param -> a -> Env a -> Env a
bindP (PVal id _) = bind id
bindP (PMut id _) = bindMut id

update :: Ident -> a -> Env a -> Maybe (Env a)
update id val env = case Map.lookup id env of
    Just (v, True, refid, ovr) -> Just (Map.insert id (val, True, refid, ovr) env)
    _                          -> Nothing

-- | Binds a list of arguments to their a in the environment.
bindParams :: [(Param, a)] -> Env a -> Env a
bindParams xs env = foldl (\acc (x, v) -> bindP x v acc) env xs

-- | Merges an dynamic env and a list defining an env. The list is of type EnvVar a.
mergeDEnv :: EnvMerge a
mergeDEnv oenv nenv = case nenv of
    [] -> oenv
    ((id, (val, ismut, _, False)):kvs) -> case Map.lookup id oenv of
        Just (_, True, _, _) | ismut -> case update id val oenv of
            Just noenv -> mergeDEnv noenv kvs
            Nothing    -> mergeDEnv oenv kvs
        _ -> mergeDEnv oenv kvs
    (_:kvs) -> mergeDEnv oenv kvs
