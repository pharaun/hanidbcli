--import Prelude hiding (FilePath)
import Data.Word (Word8)
import Numeric (showHex)

import Control.Applicative

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.List as L
import Control.Parallel.Strategies

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap

--import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString)

import Data.Conduit (($$), runResourceT)
import qualified Data.Conduit.List as CL
import Data.Conduit.Filesystem (sourceFile, traverse)
import Crypto.Conduit (sinkHash, hashFile)

import Crypto.Hash.Ed2k (ED2K, hashlazy, initEd2k, updateEd2k, finalizeEd2k, Ctx)
import Data.Serialize (encode)

--import HClient.Options
--import System.Console.CmdArgs
--
--main :: IO ()
--main = print =<< cmdArgsRun optionMode
import qualified Data.ByteString.Lazy as BS
import System.IO
import Control.Exception (bracket, evaluate)

-- Strictness
import Control.DeepSeq

-- Benchmarks
import Criterion.Main

-- MD4 parallel impl
import qualified Crypto.Hash.MD4 as MD4

-- MMap'd file
import System.IO.Posix.MMap.Lazy

instance NFData Ctx where
    rnf a = a `seq` ()

testFile :: FilePath
testFile = "test.mkv"

main :: IO ()
main = do
    putStrLn "Warmup"
    a <- test1 testFile
    putStrLn a
    a <- test3 testFile
    putStrLn a

--    defaultMain
--        [ bgroup "ed2k list hash"
----            [ bench "ed2k foreach" $ nf test1
----            [ bench "ed2k conduit" $ test2 testFile
----            [ bench "ed2k foreach" $ test1 testFile
--            [ bench "ed2k parMap" $ test3 testFile >>= putStrLn
--            ]
--        ]



-- TODO: this gets great performance but hashes in 32kB chunks not 9.27MiB chunks
-- Solution seems to be forking LazyByteString to use 9.27 MiB chunks with forked mmap.lazy
test3 :: FilePath -> IO String
test3 file = do
    d <- unsafeMMapFile file
    let ctxs = parMap rdeepseq MD4.hash (BS.toChunks d)
        ctx  = L.foldl' MD4.update MD4.init ctxs
    return $ fileLine file $ MD4.finalize ctx



-- Doing the IO myself, in managed strict hGet (9.27MiB) chunks
test1 :: FilePath -> IO (String)
test1 file = do
    t <- mapM (\path -> bracket (openFile path ReadMode) hClose (\fh -> foreach fh initEd2k)) [file]
    return $ fileLine file $ head t
    where
        foreach :: Handle -> Ctx -> IO S.ByteString
        foreach fh ctx = do
            a <- S.hGet fh (9500*1024)
            case S.null a of
                True  -> return $ finalizeEd2k ctx
                -- Then also a deepseq here to force the foreach to release
                -- the chunks of bytestring that it wants to rentain
                False -> foreach fh $!! (updateEd2k ctx a)


-- Using conduit hashFile -- Still unusably slow
test2 :: FilePath -> IO (String)
test2 file = do
    digest <- hashFile file
    let hash = toHex . encode $ (digest :: ED2K)
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
