module Env (Env, emptyEnv, beginScope, endScope, assoc, find) where

import Data.Map as M

type Env a = [M.Map String a]

emptyEnv :: Env a
emptyEnv = []

beginScope :: Env a -> Env a
beginScope env = M.empty : env

endScope :: Env a -> Env a
endScope [] = error "Empty Environment"
endScope (local:env) = env

assoc:: String -> a -> Env a -> Env a
assoc _ _ [] = error "Empty Environment"
assoc s v (local:env) = (M.insert s v local):env

find :: String -> Env a -> Maybe a
find _ [] = Nothing
find s (local:env) | M.member s local = Just $ local M.! s
find s (local:env) = find s env
