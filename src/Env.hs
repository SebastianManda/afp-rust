module Env where

import qualified Data.Map as Map
import Lang.Abs ( Ident )

type Env a = Map.Map Ident (a, Bool)

empty :: Env a
empty = Map.empty

find :: Ident -> Env a -> Maybe a
find id env =  case Map.lookup id env of
    Just (v, _) -> Just v
    Nothing      -> Nothing

bind :: Ident -> a -> Env a -> Env a
bind id val = Map.insert id (val, False)

bindMut :: Ident -> a -> Env a -> Env a
bindMut id val = Map.insert id (val, True)

update :: Ident -> a -> Env a -> Maybe (Env a)
update id val env = case Map.lookup id env of
    Just (v, True) -> Just (Map.insert id (val, True) env)
    _              -> Nothing

-- | Merges an env and a list defining an env. The list is a list of tuples (Ident, (a, Bool)).
merge :: Env a -> [(Ident, (a, Bool))] -> Env a
merge oenv nenv = case nenv of
    [] -> oenv
    ((k, (v, b)):kvs) -> case Map.lookup k oenv of
        Just (_, True) | b -> case update k v oenv of
            Just noenv -> merge noenv kvs
            Nothing    -> merge oenv kvs
        _ -> merge oenv kvs