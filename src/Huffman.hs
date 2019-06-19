module Huffman
    ( main
    ) where

import Weighted
import qualified Heap
import qualified PrefixTree
import qualified Data.Map.Strict as M

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
    pushLeaf c i h = Heap.push (PrefixTree.weightedLeaf i c) h

merge2 :: CHeap -> Maybe CHeap
merge2 h = do
  (x0, h0) <- Heap.pop h
  (x1, h1) <- Heap.pop h0
  let branch = PrefixTree.weightedBranch x0 x1
  return $ Heap.push branch h1

buildTree :: CHeap -> PrefixTree.PrefixTree Char
buildTree h =
  let
    mergeRec h = case merge2 h of
      Nothing -> h
      Just h' -> mergeRec h'

    Just (Weighted _ binTree) = fmap Prelude.fst . Heap.pop $ mergeRec h
  in
    binTree

main :: IO ()
main = do
  let h = buildHeap "aaaaaabbcdef"
  let t = buildTree h
  let Just c = PrefixTree.encodeString t "abc"
  let ds = PrefixTree.decodeString c t
  print t
  print c
  print ds