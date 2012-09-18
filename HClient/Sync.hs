module HClient.Sync
    ( fileSync
    , directorySync
    , fileDirectorySync

    , UniqueFile(..)
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (($!!))
import System.IO
import Control.Monad

import System.Posix.Files (getSymbolicLinkStatus, isRegularFile, isDirectory, deviceID, fileID, fileSize)
import System.Posix.Types (DeviceID, FileID, FileOffset)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

-- "Machine unique" file identifier with some additional info such as size and hash
data UniqueFile = UniqueFile FilePath FileID DeviceID FileOffset (Maybe String)
    deriving (Show)

-- Steps to detect if a file is new or not based off fileid/deviceid is:
-- Using IxSet i think (storing the index/metadata in acid-state)
--
-- 1. Load the IxSet of known/processed files
-- 2. Farm the filesystem and for each file check to see if its in the IxSet via
--  a. Winnow down IxSet by DeviceID
--  b. Winnow down IxSet by FileID
--  c. If exists then (probably can ignore/proceed on)
--  d. If not exist then add to a list of file that needs to be processed
--  - Disclaimer this only works for file on the same device id, if there's multiple
--  - file on other devices, the only way to deal with these is to actually hash/1:1
--  - compare the files, in this case hasing is probably adequate.
-- 3. Can probably start processing the files in (2d) here
-- 4. Should have some basic cross device/fs detection, ex we have to process the hash
--    of all files anyway, so might as well do some quick duplication detection and warn
--    user of duplicates/store info on who/which files duplicates which
--
-- 5. Should also deal with files that has been deleted, for this we would need to build
--    up an entire set of processed (new) plus existing file and find the files that do not
--    exist.
--
--    - Can probably do that during step 2a-d via  having a "dynamic?" set that over time has
--    - entries removed out of it so that when all done what's left is the deleted/unavailable
--    - files

fileSync :: [FilePath] -> IO [UniqueFile]
fileSync = fileDirectorySync

fileDirectorySync :: [FilePath] -> IO [UniqueFile]
fileDirectorySync paths = concat <$> forM paths (\path -> do
        -- TODO: figure out a good approach for dealing with hard and symbolic links
        fs <- getSymbolicLinkStatus path

        if isRegularFile fs
        then return [UniqueFile path (fileID fs) (deviceID fs) (fileSize fs) Nothing]
        else if isDirectory fs
            then (directorySync [path] >>= return)
            else return [])

directorySync :: [FilePath] -> IO [UniqueFile]
directorySync dirs = concat <$> forM dirs getRecursiveContents
    where
        getRecursiveContents :: FilePath -> IO [UniqueFile]
        getRecursiveContents topdir = do
            a <- properNames topdir
            concat <$> forM a (\name -> do
                let path = topdir </> name
                fs <- getSymbolicLinkStatus path
                if isDirectory fs
                then getRecursiveContents path
                else (fileSync [path] >>= return))
            where
                properNames topdir = do
                    names <- getDirectoryContents topdir
                    return $ filter (`notElem` [".", ".."]) names

