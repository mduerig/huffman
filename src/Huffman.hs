module Huffman
    ( main
    ) where

import qualified Data.Map.Strict as M ( Map
                                      , empty
                                      , insertWith
                                      , foldrWithKey
                                      )

import Weighted     ( Weighted(Weighted))
import Heap         ( Heap
                    , pop
                    , push
                    , emptyHeap
                    )
import PrefixTree   ( WeightedTree
                    , PrefixTree
                    , weightedLeaf
                    , weightedBranch
                    , encodeString
                    , decodeString
                    )

type FreqTable a = M.Map a Int

frequencies :: Ord a => [a] -> FreqTable a
frequencies = foldr f M.empty
  where
    f x m = M.insertWith (+) x 1 m

type CHeap = Heap (WeightedTree Char)

buildHeap :: String -> CHeap
buildHeap s = M.foldrWithKey pushLeaf emptyHeap (frequencies s)
  where
    pushLeaf c i h = push (weightedLeaf i c) h

merge2 :: CHeap -> Maybe CHeap
merge2 h = do
  (x0, h0) <- pop h
  (x1, h1) <- pop h0
  let branch = weightedBranch x0 x1
  return $ push branch h1

buildTree :: CHeap -> PrefixTree Char
buildTree h =
  let
    mergeRec h = case merge2 h of
      Nothing -> h
      Just h' -> mergeRec h'

    Just (Weighted _ binTree) = fmap Prelude.fst . pop $ mergeRec h
  in
    binTree

main :: IO ()
main = do
  let h = buildHeap "aaaaaabbcdef"
  let t = buildTree h
  let Just c = encodeString t "abc"
  let ds = decodeString c t
  print t
  print c
  print ds