module Memory (Ref, Mem, emptyMem, new, set, get, free) where

import Data.Map as M

type Ref = Int

type Mem a = M.Map Ref a

emptyMem :: Mem a
emptyMem = M.empty

new :: a -> Mem a -> (Ref,Mem a)
new v mem = (r, M.insert r v mem)
            where r = M.size mem

set :: Ref -> a -> Mem a -> Mem a
set = M.insert

get :: Ref -> Mem a -> a
get = flip (M.!)

free :: Ref -> Mem a -> Mem a
free = M.delete
