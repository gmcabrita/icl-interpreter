module Semantics where

import Syntax
import Env
import qualified Memory as M
import System.IO.Unsafe (unsafePerformIO)

type Mem = M.Mem Result

-- Expression eval function

eval_exp :: ASTExpression -> Env Result -> Mem -> (Result, Mem)

eval_exp (Num n) _ mem = (Integer n, mem)

eval_exp (Str s) _ mem = (String s, mem)

eval_exp (Id s) env mem =
  case find s env of
    Just v  ->  (v, mem)
    Nothing ->  (Undefined, mem)

eval_exp (Add e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer n, Integer n') ->  (Integer (n + n'), mem'')
    _                       ->  (Undefined, mem'')

eval_exp (Multiply e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer n, Integer n') ->  (Integer (n * n'), mem'')
    _                       ->  (Undefined, mem'')

eval_exp (Subtract e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer n, Integer n') ->  (Integer (n - n'), mem'')
    _                       ->  (Undefined, mem'')

eval_exp (Divide e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer n, Integer n') ->  (Integer (n `div` n'), mem'')
    (Integer _, Integer 0)  ->  (Undefined, mem'')
    _                       ->  (Undefined, mem'')

eval_exp (VTrue) _ mem = (Boolean True, mem)

eval_exp (VFalse) _ mem = (Boolean False, mem)

eval_exp (And e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v,v') of
    (Boolean n, Boolean n') ->  (Boolean (n && n'), mem'')
    _                       ->  (Undefined,mem'')

eval_exp (Or e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Boolean n, Boolean n') ->  (Boolean (n || n'), mem'')
    _                       ->  (Undefined,mem'')

eval_exp (Not e) env mem =
  let (v, mem') = c_eval e env mem in
  case v of
    Boolean n ->  (Boolean (not n), mem')
    _         ->  (Undefined, mem')

eval_exp (Let e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer v, Integer v') ->  (Boolean (v <= v'), mem'')
    (_, _)                  ->  (Undefined, mem'')

eval_exp (Lt e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer v, Integer v') ->  (Boolean (v < v'), mem'')
    (_, _)                  ->  (Undefined, mem'')

eval_exp (Gt e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer v, Integer v') ->  (Boolean (v > v'), mem'')
    (_, _)                  ->  (Undefined, mem'')

eval_exp (Get e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Integer v, Integer v') ->  (Boolean (v >= v'), mem'')
    (_, _)                  ->  (Undefined, mem'')

eval_exp (Eq e e') env mem =
  let (v, mem') = c_eval e env mem in
  let (v', mem'') = c_eval e' env mem' in
  case (v, v') of
    (Undefined, _)  ->  (Undefined, mem'')
    (_, Undefined)  ->  (Undefined, mem'')
    (v, v')         ->  (Boolean (v == v'), mem'')

eval_exp (Ternary e e' e'') env mem =
  let (v, mem') = c_eval e env mem in
  case v of
    Boolean b | b ->  eval_exp e' env mem'
    Boolean b     ->  eval_exp e'' env mem'
    _             ->  (Undefined, mem')

eval_exp (Alloc e) env mem = (Reference ref, mem'')
  where
    (v, mem') = c_eval e env mem
    (ref, mem'') = M.new v mem'

eval_exp (Deref e) env mem = (M.get ref mem', mem')
  where
    (Reference ref, mem') = c_eval e env mem

eval_exp (LDecl decls s e) env mem = eval_exp e new_env mem''
  where
    env' = beginScope env
    (new_env, new_mem) =  foldl (\(env', mem') (x, e') ->
                                 let (m1, m2) = eval_exp e' env' mem' in
                                 (assoc x m1 env', m2))
                          (env', mem) decls
    (_, mem'') = eval_state s new_env new_mem

eval_exp (Lambda args body) env mem = (Closure (map fst args) body env, mem)

eval_exp (Apply e args) env mem =
  case c_eval e env mem of
    (Closure d_args e'' env', mem') ->  eval_exp e'' new_env new_mem
                                        where
                                          env'' = beginScope env'
                                          (new_env, new_mem) =
                                            foldl (\(env', mem') (x, e') ->
                                                   let (m1, m2) = (Delay e' env', mem') in
                                                   (assoc x m1 env', m2))
                                            (env'', mem') (zip d_args args)
    (_, mem')                       ->  (Undefined, mem')

eval_exp (Object fields) env mem =
  if Undefined `elem` results
    then (Undefined,mem)
  else (object, mem)
  where
    object = ObjectR $ zip labels results
    results = map (\e -> fst $ eval_exp e envr mem) fieldExps
    envr = assoc "this" object $ beginScope env
    (labels,fieldExps) = unzip fields

eval_exp (Select e label) env mem =
  case c_eval e env mem of
    (ObjectR fields,mem') ->  (head [ v | (l,v) <- fields, l == label], mem')
    (_,mem')              ->  (Undefined,mem')

eval_exp (ListDecl es) env mem = (list, mem)
  where
    list = List elements
    elements = map (\e -> fst $ eval_exp e env mem) es

eval_exp (ListSelect e e') env mem =
  if pos >= 0 && pos < len
    then (list !! pos, mem'')
  else (Undefined, mem'')
  where
    (List list, mem') = c_eval e env mem
    (Integer pos, mem'') = c_eval e' env mem'
    len = length list

eval_exp (ListConcat e e') env mem =
  case (v, v') of
    (List l, List l') ->  (List (l ++ l'), mem'')
    (_, _)            ->  (Undefined, mem'')
  where
    (v, mem') = c_eval e env mem
    (v', mem'') = c_eval e' env mem'

-- State eval function

eval_state :: ASTStatement -> Env Result -> Mem -> (Result, Mem)

eval_state (Free e) env mem = (Boolean True, mem'')
  where
    (Reference ref, mem') = c_eval e env mem
    mem'' = M.free ref mem'

eval_state (Print e) env mem =
  case v of
    Integer x ->  unsafePerformIO $ do { putStr $ show x; return (Boolean True, mem') }
    Boolean x ->  unsafePerformIO $ do { putStr $ show x; return (Boolean True, mem') }
    String x  ->  unsafePerformIO $ do { putStr x; return (Boolean True, mem') }
    _         ->  (Boolean False, mem')
  where
    (v, mem') = c_eval e env mem

eval_state (Println) env mem =
  unsafePerformIO $ do { putStrLn ""; return (Boolean True, mem) }

eval_state (Seq s s') env mem =
  case val of
    Boolean True  ->  eval_state s' env mem'
    _             ->  (Boolean False, mem')
  where
    (val, mem') = eval_state s env mem

eval_state (SDecl decls s) env mem = eval_state s new_env new_mem
  where
    env' = beginScope env
    (new_env, new_mem) =  foldl (\(env', mem') (x, e') ->
                                 let (m1, m2) = eval_exp e' env' mem' in
                                 (assoc x m1 env', m2))
                          (env', mem) decls

eval_state (Assign e e') env mem =
  case c_eval e env mem of
    (Reference ref, mem') -> (Boolean True, mem''')
                              where
                                (v, mem'') = c_eval e' env mem'
                                mem''' = M.set ref v mem''
    (_, mem')             -> (Boolean False, mem')

eval_state (IfElse e s s') env mem =
  if cond
    then eval_state s env mem'
    else eval_state s' env mem'
  where
    (Boolean cond, mem') = c_eval e env mem

eval_state (If e s) env mem =
  if cond
    then eval_state s env mem'
    else (Boolean False, mem')
  where
    (Boolean cond, mem') = c_eval e env mem

eval_state (While e s) env mem =
  case cond of
    Boolean True  ->  eval_state (While e s) env (snd $ eval_state s env mem')
    Boolean False ->  (Boolean True, mem')
    _             ->  (Boolean False, mem')
  where
    (cond, mem') = c_eval e env mem

eval_state (Call e args) env mem = eval_exp (Apply e args) env mem

eval_state (For s e e') env mem =
  (Boolean True, mem'')
  where
    new_env = beginScope env
    (List list, mem') = c_eval e env mem
    (_, mem'')  = foldr (\x (env, mem) ->
                          let new_env = assoc s x env in
                          (new_env, snd $ eval_exp e' new_env mem))
                  (new_env, mem') list

eval_state (ForFilter s e e' e'') env mem =
  (Boolean True, mem'')
  where
    new_env = beginScope env
    (List list, mem') = c_eval e env mem
    (_, mem'')  = foldr (\x (env, mem) ->
                          let new_env = assoc s x env in
                          let (Boolean filter, mem') = c_eval e' new_env mem in
                          if filter
                            then (new_env, snd $ eval_exp e'' new_env mem)
                          else (new_env, mem'))
                  (new_env, mem') list

for s list e' env mem =
  case list of
    []    ->  (Undefined, mem)
    [x]   ->  eval_exp e' new_env mem
              where
                new_env = assoc s x env
    x:xs  ->  for s xs e' new_env mem'
              where
                new_env = assoc s x env
                (_, mem') = eval_exp e' new_env mem

for_filter s list e' e'' env mem =
    case list of
    []    ->  (Undefined, mem)
    [x]   ->  if filter
                then eval_exp e' new_env mem'
              else (Undefined, mem')
              where
                new_env = assoc s x env
                (Boolean filter, mem') = c_eval e'' new_env mem
    x:xs  ->  for_filter s xs e' e'' new_env mem''
              where
                new_env = assoc s x env
                (Boolean filter, mem') = c_eval e'' new_env mem
                (_, mem'') =  if filter
                                then eval_exp e' new_env mem'
                              else (Undefined, mem')


-- Concrete eval

c_eval :: ASTExpression -> Env Result -> Mem -> (Result, Mem)
c_eval e env mem  =
  case eval_exp e env mem of
    (Delay e' env', mem') ->  c_eval e' env' mem'
    (v, mem')             ->  (v, mem')
