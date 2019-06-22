module Encode where

import System.IO
import Data.ByteString                (ByteString)
import Data.Bits
import Data.Binary hiding             (encodeFile)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PS

import Lens.Family2

import Weighted
import PrefixTree
import Heap

type FreqTable a = M.Map a Int
type PHeap a = Heap (WeightedTree a)

merge2 :: PHeap a -> Maybe (PHeap a)
merge2 h = do
    (x0, h0) <- pop h
    (x1, h1) <- pop h0
    let branch = weightedBranch x0 x1
    return $ push branch h1

buildTree :: PHeap a -> Maybe (WeightedTree a)
buildTree h =
    let
        mergeRec h = case merge2 h of
            Nothing -> h
            Just h' -> mergeRec h'
    in
        fmap Prelude.fst . pop $ mergeRec h

buildHeap :: FreqTable a -> PHeap a
buildHeap = M.foldrWithKey pushLeaf emptyHeap
    where
        pushLeaf a w h = push (weightedLeaf w a) h

frequencies :: Ord a => Producer a IO () -> IO (FreqTable a)
frequencies =
    PP.fold countOccurrence M.empty id
    where
        countOccurrence m x = M.insertWith (+) x 1 m

toWord8 :: Monad m => Pipe ByteString Word8 m r
toWord8 = PP.mapFoldable B.unpack

analyseFile :: FilePath -> IO (Maybe (WeightedTree Word8))
analyseFile inFile =
    withFile inFile ReadMode $ \h -> do
        let bytes = PS.fromHandle h >-> toWord8
        fqs <- frequencies bytes
        let heap = buildHeap fqs
        return $ buildTree heap

encodeBytes :: Monad m => PrefixTree Word8 -> Pipe ByteString Direction m r
encodeBytes hTree = PP.mapFoldable (enc . B.unpack)
    where
        enc ws = concat (encodeString hTree ws)

encodeDirections :: Monad m => Int -> Pipe Direction Word8 m ()
encodeDirections len = readDirs len 0 0
    where
        readDirs 0 _ w = yield w
        readDirs l 8 w = yield w
        readDirs l n w = do
            d <- await
            readDirs (l - 1) (n + 1)
                (if d == DLeft then w
                 else setBit w n)

encodeFile :: FilePath -> FilePath -> Int -> PrefixTree Word8 -> IO ()
encodeFile inFile outFile len hTree =
    let
        convertToByteString = view PS.pack
    in
        withFile  inFile ReadMode  $ \hIn  ->
        withFile outFile WriteMode $ \hOut -> do
            runEffect $
                PS.fromLazy (encode (len, hTree))
                    >-> PS.toHandle hOut
            runEffect $
                convertToByteString
                    (   PS.fromHandle hIn
                    >-> encodeBytes hTree
                    >-> encodeDirections len
                    )
                >-> PS.toHandle hOut

hEncode :: FilePath -> FilePath -> IO ()
hEncode inFile outFile = do
    Just (Weighted w hTree) <- analyseFile inFile
    encodeFile inFile outFile w hTree