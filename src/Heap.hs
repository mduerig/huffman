module Heap 
    ( Heap
    , emptyHeap
    , pop
    , push
    ) where

data Heap a 
    = Empty
    | Branch a (Heap a) (Heap a)
    deriving Show
    
emptyHeap :: Heap a
emptyHeap = Empty    

heap :: Ord a => a -> Heap a
heap a = Branch a Empty Empty
        
merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty l = l
merge r Empty = r
merge bl@(Branch l ll lr) br@(Branch r rl rr)
    | l < r     = Branch l (merge lr br) ll
    | otherwise = Branch r rr (merge bl rl)

pop :: Ord a => Heap a -> Maybe (a, Heap a)
pop Empty = Nothing
pop (Branch a l r) = Just (a, merge l r)   

push :: Ord a => a -> Heap a -> Heap a
push x h = merge h (heap x)
