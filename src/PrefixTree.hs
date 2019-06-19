module PrefixTree
    ( PrefixTree (Leaf, Branch)
    , ptLeaf
    , ptBranch
    , WeightedTree
    , weightedLeaf
    , weightedBranch
    , encodeString
    , decodeString
    ) where

import qualified Data.Map.Strict as M ( Map
                                       , lookup
                                       , singleton
                                       )

import Data.List (unfoldr)
import Weighted (Weighted(Weighted))

data PrefixTree a
    = Leaf a
    | Branch (PrefixTree a) (PrefixTree a)
    deriving (Show, Eq)

ptLeaf :: a -> PrefixTree a
ptLeaf = Leaf

ptBranch :: PrefixTree a -> PrefixTree a -> PrefixTree a
ptBranch = Branch

type WeightedTree a = Weighted (PrefixTree a)

weightedLeaf :: Int -> a -> WeightedTree a
weightedLeaf i = Weighted i . Leaf

weightedBranch :: WeightedTree a -> WeightedTree a -> WeightedTree a
weightedBranch (Weighted v l) (Weighted w r) = Weighted (v + w) (Branch l r)

data Direction = DLeft | DRight
  deriving (Show, Eq)

type Encoding = [Direction]

encodingTable :: Ord a => PrefixTree a -> M.Map a Encoding
encodingTable t = encode t []
  where
    encode (PrefixTree.Leaf a)     encoding = M.singleton a (reverse encoding)
    encode (PrefixTree.Branch l r) encoding = encode l (DLeft:encoding)
                                           <> encode r (DRight:encoding)

encodeString :: Ord a => PrefixTree a -> [a] ->  Maybe Encoding
encodeString t as =
  let
    encoding = map (\k -> M.lookup k (encodingTable t))
  in
    concat <$> (sequence . encoding $ as)

decodeChar :: Encoding -> PrefixTree a -> Maybe (a, Encoding)
decodeChar ds (Leaf a) = Just(a, ds)
decodeChar (DLeft:ds ) (Branch l r) = decodeChar ds l
decodeChar (DRight:ds) (Branch l r) = decodeChar ds r
decodeChar _ _ = Nothing

decodeString :: Encoding -> PrefixTree a -> [a]
decodeString ds t = unfoldr (`decodeChar` t) ds

