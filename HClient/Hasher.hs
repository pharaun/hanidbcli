module HClient.Hasher
    ( fileHash
    , directoryHash
    , fileDirectoryHash

    , conduitFileHash
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (($!!))
import Control.Exception (bracket)
import Data.Word (Word8)
import Numeric (showHex)
import System.IO
import qualified Crypto.Hash.Ed2k as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad

import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

-- Conduit
import Control.Monad.IO.Class (liftIO, MonadIO)
import Crypto.Conduit (sinkHash)
import Data.Conduit (($$), runResourceT, MonadResource, GSource, yield, bracketP)
import Data.Serialize (encode)


fileDirectoryHash :: [FilePath] -> IO [(FilePath, String)]
fileDirectoryHash paths = concat <$> forM paths (\path -> do
        fs <- getFileStatus path

        if isRegularFile fs
        then (fileHash [path] >>= return)
        else if isDirectory fs
            then (directoryHash [path] >>= return)
            else return [(path, "")])

-- This is terrible, as is fileDirectoryHash but it'll work for now
directoryHash :: [FilePath] -> IO [(FilePath, String)]
directoryHash dirs = concat <$> forM dirs getRecursiveContents
    where
        getRecursiveContents :: FilePath -> IO [(FilePath, String)]
        getRecursiveContents topdir = do
            a <- properNames topdir
            concat <$> forM a (\name -> do
                let path = topdir </> name
                fs <- getFileStatus path
                if isDirectory fs
                then getRecursiveContents path
                else (fileHash [path] >>= return))
            where
                properNames topdir = do
                    names <- getDirectoryContents topdir
                    return $ filter (`notElem` [".", ".."]) names


-- Doing the IO myself, in managed strict hGet (1MiB chunks)
fileHash :: [FilePath] -> IO [(FilePath, String)]
fileHash files = forM files (\path -> do
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

-- Doing the IO with a custom (1024*1024) bigblock conduit sourceFile
conduitFileHash :: [FilePath] -> IO [(FilePath, String)]
conduitFileHash files = forM files (\path -> do
        digest <- runResourceT $ bigSourceFile path $$ sinkHash
        let hash = encode (digest :: E.ED2K)
        return $ fileLine path hash)

bigSourceFile :: MonadResource m => FilePath -> GSource m B.ByteString
bigSourceFile file = bracketP (openBinaryFile file ReadMode) hClose sourceHandle

sourceHandle :: MonadIO m => Handle -> GSource m B.ByteString
sourceHandle h = loop
    where
        loop = do
            bs <- liftIO (B.hGetSome h (1024*1024))
            if B.null bs
            then return ()
            else yield bs >> loop


fileLine :: FilePath -> B.ByteString -> (FilePath, String)
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
