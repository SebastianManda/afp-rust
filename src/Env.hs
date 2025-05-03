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
