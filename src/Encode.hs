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
        mergeRec h = maybe h mergeRec (merge2 h)
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

analyseFile :: FilePath -> IO (Maybe (WeightedTree Word8))
analyseFile inFile =
    let
        toWord8 = PP.mapFoldable B.unpack
    in
        withFile inFile ReadMode $ \h -> do
            let bytes = PS.fromHandle h >-> toWord8
            fqs <- frequencies bytes
            let heap = buildHeap fqs
            return $ buildTree heap

encodeBytes :: Monad m => PrefixTree Word8 -> Pipe ByteString Direction m r
encodeBytes hTree = PP.mapFoldable (enc . B.unpack)
    where
        encoding = encodeChars hTree
        enc ws = concat (encodeString encoding ws)

parseDirection :: Monad m => Parser Direction m (Maybe Word8)
parseDirection = parse 0 0
    where
        parse 8 w = return $ Just w
        parse n w = do
            d <- draw
            case d of
                Just DLeft          -> parse (n + 1) w
                Just DRight         -> parse (n + 1) (setBit w n)
                Nothing | n == 0    -> return Nothing
                        | otherwise -> return $ Just w

parseDirections :: Monad m => Producer Direction m r -> Producer Word8 m ()
parseDirections dirs = do
    (dir, leftover) <- lift $ runStateT parseDirection dirs
    case dir of
        Just word8 -> do
            yield word8
            parseDirections leftover
        Nothing -> return ()

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

            let directions = PS.fromHandle hIn
                    >-> encodeBytes hTree

            runEffect $
                (convertToByteString . parseDirections) directions
                    >-> PS.toHandle hOut

hEncode :: FilePath -> FilePath -> IO ()
hEncode inFile outFile = do
    Just (Weighted w hTree) <- analyseFile inFile
    encodeFile inFile outFile w hTree