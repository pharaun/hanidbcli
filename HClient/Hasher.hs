import Data.Word (Word8)
import Numeric (showHex)

import Control.Applicative

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import qualified Crypto.Hash.Ed2k as E

import qualified Data.ByteString.Lazy as BS
import System.IO
import Control.Exception (bracket)

-- Strictness
import Control.DeepSeq

-- Conduit
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Filesystem (sourceFile, traverse)
import Crypto.Conduit (sinkHash, hashFile)
import Data.Serialize (encode)


main :: IO ()
main = do
    putStrLn "Warmup"
    a <- test1 testFile
    putStrLn a
    a <- test2 testFile
    putStrLn a


-- Doing the IO myself, in managed strict hGet (9.27MiB) chunks
test1 :: FilePath -> IO (String)
test1 file = do
    t <- mapM (\path -> bracket (openFile path ReadMode) hClose (\fh -> foreach fh E.initEd2k)) [file]
    return $ fileLine file $ head t
    where
        foreach :: Handle -> E.Ctx -> IO S.ByteString
        foreach fh ctx = do
            let size = 1024 * 1024 -- Found via trial runs (1MiB)
            a <- S.hGet fh size
            case S.null a of
                True  -> return $ E.finalizeEd2k ctx
                -- Then also a deepseq here to force the foreach to release
                -- the chunks of bytestring that it wants to rentain
                False -> foreach fh $!! (E.updateEd2k ctx a)

-- Conduit - Ed2k
test2 :: FilePath -> IO (String)
test2 file = do
    digest <- hashFile file
    let hash = toHex . encode $ (digest :: E.ED2K)
    return $ fileLine file hash



fileLine :: FilePath -> S.ByteString -> String
fileLine path c = hash c ++ " " ++ path

hash :: S.ByteString -> String
hash = show . toHex

-- Overall, this function is pretty inefficient. Writing an optimized version
-- in terms of unfoldR is left as an exercise to the reader.
toHex :: S.ByteString -> S.ByteString
toHex =
    S.concatMap word8ToHex
  where
    word8ToHex :: Word8 -> S.ByteString
    word8ToHex w = S8.pack $ pad $ showHex w []

    -- We know that the input will always be 1 or 2 characters long.
    pad :: String -> String
    pad [x] = ['0', x]
    pad s   = s
