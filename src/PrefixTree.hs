module PrefixTree 
    ( PrefixTree (Leaf, Branch)
    , ptLeaf
    , ptBranch
    , WeightedTree
    , weightedLeaf
    , weightedBranch
    ) where

import Weighted 
        
data PrefixTree a
    = Leaf a
    | Branch (PrefixTree a) (PrefixTree a)
    deriving (Show, Eq)

type WeightedTree a = Weighted (PrefixTree a)

ptLeaf :: a -> PrefixTree a
ptLeaf = Leaf

ptBranch :: PrefixTree a -> PrefixTree a -> PrefixTree a
ptBranch = Branch

weightedLeaf :: Int -> a -> WeightedTree a
weightedLeaf i = Weighted i . Leaf

weightedBranch :: WeightedTree a -> WeightedTree a -> WeightedTree a
weightedBranch (Weighted v l) (Weighted w r) = Weighted (v + w) (Branch l r)
    

