import Data.Word (Word8)
import Numeric (showHex)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import qualified Crypto.Hash.Ed2k as E

-- Benchmarks
import Criterion.Main

-- Hasher
import HClient.Hasher (fileDirectoryHash, conduitFileHash)


testFile :: FilePath
testFile = "test.mkv"

main :: IO ()
main = do
    putStrLn "Warmup"
    a <- test1 testFile
    putStrLn $ show a
    a <- test2 testFile
    putStrLn $ show a

    defaultMain
        [ bgroup "ed2k list hash"
            [ bench "ed2k foreach"  $ test1 testFile
            , bench "ed2k conduit"  $ test2 testFile
            ]
        ]


-- Doing the IO myself, in managed strict hGet (9.27MiB) chunks
test1 :: FilePath -> IO [(FilePath, String)]
test1 file = fileDirectoryHash [file]

-- Conduit - Ed2k
test2 :: FilePath -> IO [(FilePath, String)]
test2 file = conduitFileHash [file]

-- Here for next experimental benchmark stuff

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
