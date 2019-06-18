module Huffman
    ( main
    ) where

import Data.Binary     
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

main :: IO ()
main = do
  let h = buildHeap "aaaaaabbcdef"
  let t = buildTree h
  let ea = encodeChar 'a' t
  let eb = encodeChar 'b' t
  let ec = encodeChar 'c' t
  print ea
  print eb
  print ec
