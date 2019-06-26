module Decode where

import System.IO
import Data.ByteString                (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Data.Binary hiding (decodeFile)

import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Binary as PB
import qualified Pipes.ByteString as PS
import Pipes.Parse

import Lens.Family2

import PrefixTree

decodeHeader :: Handle -> IO (Maybe (Int, PrefixTree Word8, Producer ByteString IO ()))
decodeHeader hIn = do
    let inStr = PS.fromHandle hIn
    parseResult <- runStateT PB.decode inStr
    case parseResult of
        (Right (len, hTree), bytes) -> return $ Just (len, hTree, bytes)
        _                           -> return Nothing

toWord8 :: Pipe ByteString Word8 IO ()
toWord8 = PP.mapFoldable B.unpack

toDirection :: Pipe Word8 Direction IO ()
toDirection = do
    w <- await
    writeDirs w 0
    toDirection
    where
        writeDirs _ 8 = return ()
        writeDirs w n = do
            if testBit w n
                then yield DRight
                else yield DLeft
            writeDirs w (n + 1)

decodeDirections :: Int -> PrefixTree Word8 -> Pipe Direction Word8 IO ()
decodeDirections len hTree = decodeChar len hTree
    where
        decodeChar 0 _ = return()
        decodeChar n (Leaf a) = do
            yield a
            decodeChar (n - 1) hTree
        decodeChar n (Branch l r) = do
            d <- await
            if d == DLeft
                then decodeChar n l
                else decodeChar n r

decodeFile :: Producer ByteString IO () -> Int -> PrefixTree Word8 -> FilePath -> IO ()
decodeFile bytes len hTree outFile =
    let
        convertToByteString = view PS.pack
    in
        withFile outFile WriteMode $ \hOut ->
            runEffect $
                convertToByteString
                    (   bytes
                    >-> toWord8
                    >-> toDirection
                    >-> decodeDirections len hTree
                    )
                >-> PS.toHandle hOut

hDecode :: FilePath -> FilePath -> IO ()
hDecode inFile outFile =
    withFile inFile ReadMode $ \hIn -> do
        Just (len, hTree, bytes) <- decodeHeader hIn
        decodeFile bytes len hTree outFile
