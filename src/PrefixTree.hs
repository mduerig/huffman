module PrefixTree 
    ( WeightedTree
    , BinTree (Leaf, Branch)
    , leaf
    , branch
    ) where

import Weighted 
        
data BinTree a
    = Leaf a
    | Branch (BinTree a) (BinTree a)
    deriving (Show, Eq)

type WeightedTree a = Weighted (BinTree a)

leaf :: Int -> a -> WeightedTree a
leaf i = Weighted i . Leaf

branch :: WeightedTree a -> WeightedTree a -> WeightedTree a
branch (Weighted v l) (Weighted w r) = Weighted (v + w) (Branch l r)
    

