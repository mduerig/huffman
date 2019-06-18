module PrefixTree 
    ( WeightedTree
    , leaf
    , branch
    ) where

data BinTree a
    = Leaf a
    | Branch (BinTree a) (BinTree a)
    deriving Show

data Weighted a = Weighted Int a deriving Show

instance Eq (Weighted a) where
    Weighted v _ == Weighted w _ = v == w

instance Ord (Weighted a) where
    compare (Weighted v _) (Weighted w _) = compare v w 

type WeightedTree a = Weighted (BinTree a)

leaf :: Int -> a -> WeightedTree a
leaf i = Weighted i . Leaf

branch :: WeightedTree a -> WeightedTree a -> WeightedTree a
branch (Weighted v l) (Weighted w r) = Weighted (v + w) (Branch l r)
    

