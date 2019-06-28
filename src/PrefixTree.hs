module PrefixTree
    ( PrefixTree (Leaf, Branch)
    , ptLeaf
    , ptBranch
    , WeightedTree
    , weightedLeaf
    , weightedBranch
    , encodeChars
    , encodeString
    , decodeString
    , Direction (DLeft, DRight)
    , Encoding
    ) where

import Data.Binary
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

instance Binary a => Binary (PrefixTree a) where
  put (Leaf a) = do
    put True
    put a

  put (Branch l r) = do
    put False
    put l
    put r

  get = do
    isLeaf <- get
    if isLeaf
      then Leaf <$> get
      else Branch <$> get <*> get

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

encodeChars :: Ord a => PrefixTree a -> a -> Maybe Encoding
encodeChars t = \k -> M.lookup k (encode t [])
  where
    encode (PrefixTree.Leaf a)     encoding = M.singleton a (reverse encoding)
    encode (PrefixTree.Branch l r) encoding = encode l (DLeft:encoding)
                                           <> encode r (DRight:encoding)

encodeString :: Ord a => (a -> Maybe Encoding) -> [a] ->  Maybe Encoding
encodeString enc as = concat <$> mapM enc as

decodeChar :: Encoding -> PrefixTree a -> Maybe (a, Encoding)
decodeChar ds (Leaf a) = Just(a, ds)
decodeChar (DLeft:ds ) (Branch l r) = decodeChar ds l
decodeChar (DRight:ds) (Branch l r) = decodeChar ds r
decodeChar _ _ = Nothing

decodeString :: Encoding -> PrefixTree a -> [a]
decodeString ds t = unfoldr (`decodeChar` t) ds

