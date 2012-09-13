--import Prelude hiding (FilePath)
import Data.Word (Word8)
import Numeric (showHex)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

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

instance NFData Ctx where
    rnf a = a `seq` ()

main :: IO ()
main = do
    let file = "test.mkv"

    mapM_ (\path -> bracket (openFile path ReadMode) hClose (\fh -> do
        z <- foreach fh $!! initEd2k
        --z <- finalizeEd2k $ foldl updateEd2k initEd2k
        putStrLn $ fileLine path z
        )) [file]

    where
        foreach :: Handle -> Ctx -> IO S.ByteString
        foreach fh ctx = do
            a <- S.hGet fh (9500*1024)
            case S.null a of
                True  -> return $ finalizeEd2k ctx
                -- Then also a deepseq here to force the foreach to release
                -- the chunks of bytestring that it wants to rentain
                False -> foreach fh $!! (updateEd2k ctx a)

--    mapM_ (\path -> putStrLn . fileLine path =<< BS.readFile path) [file]

fileLine :: FilePath -> S.ByteString -> String
fileLine path c = hash c ++ " " ++ path

hash :: S.ByteString -> String
hash = show . toHex-- . hashlazy

--main :: IO ()
--main = do
--    let folder = "./test/a.mkv"
--
--    digest <- hashFile folder
--    let hash = toHex . encode $ (digest :: ED2K)
--    putStrLn $ show hash

--    md5Map <- buildMap $ decodeString folder
--    (putStrLn . show) `mapM_` md5Map
--
--buildMap :: FilePath -> IO [(S.ByteString, FilePath)]
--buildMap dir =
--    traverse False dir $$ CL.foldM addFP []
--  where
--    addFP :: [(S.ByteString, FilePath)] -> FilePath -> IO [(S.ByteString, FilePath)]
--    addFP hmap fp = do
--        digest <- runResourceT $ sourceFile fp $$ sinkHash
--        let hash = toHex . encode $ (digest :: ED2K)
--        putStrLn $ show hash
--        return $ hmap ++ [(hash, fp)]

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
