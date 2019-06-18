module Huffman
    ( main
    ) where

import Data.Binary
import Data.List
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Heap
import qualified PrefixTree

type FreqTable a = M.Map a Int

frequencies :: Ord a => [a] -> FreqTable a
frequencies = foldr f M.empty
  where
    f x m = M.insertWith (+) x 1 m

type HTree = PrefixTree.WeightedTree Char
type CHeap = Heap.Heap HTree

buildHeap :: String -> CHeap
buildHeap s = M.foldrWithKey pushLeaf Heap.empty (frequencies s)
  where
    pushLeaf c i h = Heap.push (PrefixTree.leaf i c) h

merge2 :: CHeap -> Maybe CHeap
merge2 h = do
  (x0, h0) <- Heap.pop h
  (x1, h1) <- Heap.pop h0
  let branch = PrefixTree.branch x0 x1
  return $ Heap.push branch h1

buildTree :: CHeap -> PrefixTree.BinTree Char
buildTree h =
  let
    mergeRec h = case merge2 h of
      Nothing -> h
      Just h' -> mergeRec h'

    Just (PrefixTree.Weighted _ binTree) = fmap Prelude.fst . Heap.pop $ mergeRec h
  in
    binTree

putTree :: Binary a => PrefixTree.BinTree a -> Put
putTree (PrefixTree.Leaf a) = do
  put True
  put a
putTree (PrefixTree.Branch l r) = do
  put False
  put l
  put r

getTree :: Binary a => Get (PrefixTree.BinTree a)
getTree = do
  isLeaf <- get
  if isLeaf
    then PrefixTree.Leaf <$> get
    else PrefixTree.Branch <$> get <*> get

instance Binary a => Binary (PrefixTree.BinTree a) where
  put = putTree
  get = getTree

data Direction = DLeft | DRight
  deriving (Show, Eq)

type Encoding = [Direction]

encodeChar :: Eq a => a -> PrefixTree.BinTree a -> Maybe Encoding
encodeChar c t = encode t []
  where
    encode (PrefixTree.Leaf a) encoding | a == c    = Just (reverse encoding)
                                        | otherwise = Nothing
    encode (PrefixTree.Branch l r) encoding =  encode l (DLeft:encoding)
                                           <|> encode r (DRight:encoding)

encodingTable :: Ord a => PrefixTree.BinTree a -> M.Map a Encoding
encodingTable t = encode t []
  where
    encode (PrefixTree.Leaf a)     encoding = M.singleton a (reverse encoding)
    encode (PrefixTree.Branch l r) encoding = encode l (DLeft:encoding)
                                           <> encode r (DRight:encoding)

encodeString :: Ord a => (M.Map a Encoding) -> [a] ->  Maybe Encoding
encodeString m as =
  let
    enc = map (\k -> M.lookup k m)
  in
    concat <$> (sequence . enc $ as)

decodeChar :: Encoding -> PrefixTree.BinTree a -> Maybe (a, Encoding)
decodeChar ds (PrefixTree.Leaf a) = Just(a, ds)
decodeChar (DLeft:ds ) (PrefixTree.Branch l r) = decodeChar ds l
decodeChar (DRight:ds) (PrefixTree.Branch l r) = decodeChar ds r
decodeChar _ _ = Nothing

decodeString :: Encoding -> PrefixTree.BinTree a -> [a]
decodeString ds t = unfoldr (`decodeChar` t) ds

main :: IO ()
main = do
  let h = buildHeap "aaaaaabbcdef"
  let t = buildTree h
  let e = encodingTable t
  let Just c = encodeString e "abc"
  let ds = decodeString c t
  print t
  print c
  print ds