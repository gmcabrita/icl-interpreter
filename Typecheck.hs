module Typecheck where

import Syntax
import Env
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Memory as M


num_typecheck :: ASTExpression -> ASTExpression -> Env Type -> Type
num_typecheck e e' env =
  case (e'', e''') of
    (IntType, IntType)  ->  IntType
    (_, _)              ->  None
  where
    e''  = typecheck_exp e env
    e''' = typecheck_exp e' env


bool_typecheck :: ASTExpression -> ASTExpression -> Env Type -> Type
bool_typecheck e e' env =
  case (e'', e''') of
    (BoolType, BoolType)  ->  BoolType
    (_, _)                ->  None
  where
    e''  = typecheck_exp e env
    e''' = typecheck_exp e' env

num_bool_typecheck :: ASTExpression -> ASTExpression -> Env Type -> Type
num_bool_typecheck e e' env =
  case (e'', e''') of
    (IntType, IntType)  ->  BoolType
    (_, _)                ->  None
  where
    e''  = typecheck_exp e env
    e''' = typecheck_exp e' env


typecheck_exp :: ASTExpression -> Env Type -> Type

typecheck_exp (Num n) _ = IntType

typecheck_exp (Str s) _ = StrType

typecheck_exp (VTrue) _ = BoolType
typecheck_exp (VFalse) _ = BoolType

typecheck_exp (Id _) [] = None
typecheck_exp (Id s) env = Maybe.fromMaybe None (find s env)

typecheck_exp (Add e e') env = num_typecheck e e' env
typecheck_exp (Multiply e e') env = num_typecheck e e' env
typecheck_exp (Subtract e e') env = num_typecheck e e' env
typecheck_exp (Divide e e') env = num_typecheck e e' env

typecheck_exp (And e e') env = bool_typecheck e e' env
typecheck_exp (Or e e') env = bool_typecheck e e' env
typecheck_exp (Not e) env =
  if e' == BoolType
    then BoolType
  else None
  where
    e' = typecheck_exp e env

typecheck_exp (Eq e e') env =
  case (e'', e''') of
    (BoolType, BoolType)  ->  BoolType
    (IntType, IntType)    ->  BoolType
    (_, _)                ->  None
  where
    e''  = typecheck_exp e env
    e''' = typecheck_exp e' env

typecheck_exp (Let e e') env = num_bool_typecheck e e' env
typecheck_exp (Lt e e') env = num_bool_typecheck e e' env
typecheck_exp (Gt e e') env = num_bool_typecheck e e' env
typecheck_exp (Get e e') env = num_bool_typecheck e e' env

typecheck_exp (Ternary e e' e'') env =
  case t of
    BoolType  ->  if t' == t''
                    then t'
                  else None
    _         ->  None
  where
    t   = typecheck_exp e env
    t'  = typecheck_exp e' env
    t'' = typecheck_exp e'' env

typecheck_exp (Alloc e) env =
  case typecheck_exp e env of
    None  ->  None
    t     ->  RefType t

typecheck_exp (Deref e) env =
  case typecheck_exp e env of
    RefType t ->  t
    _         ->  None

typecheck_exp (LDecl decls s e) env =
  case t of
    None  ->  None
    _     ->  typecheck_exp e new_env
  where
    env' = beginScope env
    new_env = foldl (\env (x, e') ->
                     let m1 = typecheck_exp e' env in
                     assoc x m1 env)
              env' decls
    t = typecheck_state s new_env

typecheck_exp (Lambda args body) env =
  case typecheck_exp body new_env of
    None  ->  None
    t     ->  FunType (map snd args) t
  where
    env' = beginScope env
    new_env = foldl (\env (x, t) ->
                     assoc x t env)
              env' args

typecheck_exp (Apply e args) env =
  case typecheck_exp e env of
    FunType d_args t  ->  if  length args == length d_args &&
                              foldl (\acc (x, y) ->
                                      (x == typecheck_exp y env) && acc)
                              True (zip d_args args)
                            then t
                          else None
    _                 ->  None

typecheck_exp (Object pairs) env =
  case List.find (== None) types of
    Just _  ->  None
    _       ->  object
  where
    object = ObjType (zip labels types)
    envr = assoc "this" object $ beginScope env
    (labels, types) = unzip $ map (\(x, e) -> (x, typecheck_exp e envr)) pairs

typecheck_exp (Select e s) env =
  case typecheck_exp e env of
    ObjType o ->  case List.lookup s o of
                    Just t  ->  t
                    _       ->  None
    _         ->  None

typecheck_exp (ListDecl (e:es)) env =
  if foldl (\acc x -> t == x ) True list
    then ListType t
  else None
  where
    list = map (`typecheck_exp` env) es
    t = typecheck_exp e env

typecheck_exp (ListSelect e e') env =
  case (typecheck_exp e env, typecheck_exp e' env) of
    (ListType t, IntType) ->  t
    _                     ->  None

typecheck_exp (ListConcat e e') env =
    case (typecheck_exp e env, typecheck_exp e' env) of
    (ListType t, ListType t') ->  if t == t'
                                    then ListType t
                                  else None
    _                         ->  None

typecheck_state :: ASTStatement -> Env Type -> Type
typecheck_state (Free e) env =
  case typecheck_exp e env of
    RefType x ->  x
    _         ->  None

typecheck_state (Print e) env =
  case typecheck_exp e env of
    IntType   ->  IntType
    StrType   ->  StrType
    BoolType  ->  BoolType
    _         ->  None

typecheck_state (Println) env = StrType

typecheck_state (Seq s s') env =
  case t of
    None  ->  None
    t     ->  typecheck_state s' env
  where
    t = typecheck_state s env

typecheck_state (SDecl decls s) env =
  case t of
    None  ->  None
    t     ->  t
  where
    env' = beginScope env
    new_env = foldl (\env (x, e') ->
                     let m1 = typecheck_exp e' env' in
                     assoc x m1 env')
              env' decls
    t = typecheck_state s new_env

typecheck_state (Assign e e') env =
  case typecheck_exp e env of
    RefType t ->  if t == typecheck_exp e' env
                    then t
                  else None
    _         ->  None

typecheck_state (If e s) env =
  case typecheck_exp e env of
    BoolType  ->  BoolType
    _         ->  None

typecheck_state (IfElse e s s') env =
  case typecheck_exp e env of
    BoolType  ->  BoolType
    _         ->  None

typecheck_state (While e s) env =
  case typecheck_exp e env of
    BoolType  ->  BoolType
    _         ->  None

typecheck_state (Call e s) env = typecheck_exp (Apply e s) env

typecheck_state (For s e e') env =
  case typecheck_exp e env of
    ListType t  ->  let new_env = assoc s t env in
                    typecheck_exp e' new_env
    _           ->  None

typecheck_state (ForFilter s e e' e'') env =
  case typecheck_exp e env of
    ListType t  ->  let new_env = assoc s t env in
                    let t2 = typecheck_exp e' new_env in
                    case t2 of
                      BoolType  ->  typecheck_exp e'' new_env
                      _         ->  None
    _                       ->  None
