module Weighted ( Weighted (Weighted) ) where 

data Weighted a = Weighted Int a 
    deriving Show

instance Eq (Weighted a) where
    Weighted v _ == Weighted w _ = v == w

instance Ord (Weighted a) where
    compare (Weighted v _) (Weighted w _) = compare v w 
    