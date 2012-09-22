module HClient.Hasher
    ( fileDirectoryHash
    , traditionalFileHash
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (($!!))
import Control.Exception (bracket)
import Data.Word (Word8)
import Numeric (showHex)
import System.IO hiding (FilePath)
import qualified Crypto.Hash.Ed2k as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad

import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (combine)

-- Conduit
import Control.Monad.IO.Class (liftIO, MonadIO)
import Crypto.Conduit (sinkHash)
import Data.Conduit (($$), (=$), runResourceT, MonadResource, GSource, yield, bracketP)
import Data.Conduit.Filesystem (traverse)
import Data.Serialize (encode)
import qualified Data.Conduit.List as CL

-- Better FilePath
import Prelude hiding (FilePath)
import Filesystem (listDirectory)
import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString, (</>))
import System.Posix.Files (getSymbolicLinkStatus, isRegularFile, isDirectory)
import qualified System.IO as FP


-- Takes a list of FP.FilePath and convert it to real FilePath then map over it and
-- send it to directorySync where the real magic happens
fileDirectoryHash :: [FP.FilePath] -> IO [(FP.FilePath, String)]
fileDirectoryHash p = concat <$> (mapM directoryHash $ map decodeString p)

directoryHash :: FilePath -> IO [(FP.FilePath, String)]
directoryHash p =
    getSymbolicLinkStatus (encodeString p) >>= \fs ->
    if isRegularFile fs
    then fileHash p
    else if isDirectory fs
         then traverse False p $$ CL.concatMapM fileHash =$ CL.consume
         else return [("", "")]

-- Doing the IO with a custom (1024*1024) bigBlock conduit sourceFile
fileHash :: FilePath -> IO [(FP.FilePath, String)]
fileHash p = do
    digest <- runResourceT $ bigSourceFile (encodeString p) $$ sinkHash
    let hash = encode (digest :: E.ED2K)
    return [fileLine (encodeString p) hash]

-- Custom bigBlock Conduit sourceFile with 1MiB blocks
bigSourceFile :: MonadResource m => FP.FilePath -> GSource m B.ByteString
bigSourceFile file = bracketP (openBinaryFile file ReadMode) hClose sourceHandle

sourceHandle :: MonadIO m => Handle -> GSource m B.ByteString
sourceHandle h = loop
    where
        loop = do
            bs <- liftIO (B.hGetSome h (1024*1024))
            if B.null bs
            then return ()
            else yield bs >> loop


fileLine :: FP.FilePath -> B.ByteString -> (FP.FilePath, String)
fileLine path hash = (path, (show $ toHex hash))

-- TODO: fix this up, this is inefficient, write it in unfoldR
toHex :: B.ByteString -> B.ByteString
toHex = B.concatMap word8ToHex
    where
        word8ToHex :: Word8 -> B.ByteString
        word8ToHex w = C.pack $ pad $ showHex w []

        -- We know that the input will always be 1 or 2 characters long.
        pad :: String -> String
        pad [x] = ['0', x]
        pad s   = s

-- Doing the IO myself, in managed strict hGet (1MiB chunks)
traditionalFileHash :: [FP.FilePath] -> IO [(FP.FilePath, String)]
traditionalFileHash files = forM files (\path -> do
        hash <- bracket (openFile path ReadMode) hClose (\fh -> foreach fh E.initEd2k)
        return $ fileLine path hash)
    where
        foreach :: Handle -> E.Ctx -> IO B.ByteString
        foreach fh ctx = do
            let size = 1024 * 1024 -- Found via trial runs (1MiB)
            a <- B.hGet fh size
            case B.null a of
                True  -> return $ E.finalizeEd2k ctx
                -- Then also a deepseq here to force the foreach to release
                -- the chunks of bytestring that it wants to rentain
                False -> foreach fh $!! (E.updateEd2k ctx a)
