{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module HClient.Sync
    ( fileDirectorySync
    , UniqueFile(..)
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (($!!))
import System.IO
import Control.Monad
import qualified Data.IxSet as IS
import Data.Data (Data, Typeable)

import System.Posix.Types
import System.Posix.Files (getSymbolicLinkStatus, isRegularFile, isDirectory, deviceID, fileID, fileSize, FileStatus)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

-- "Posix Unique" File identifier along with file size and maybe a hash of the file
data UniqueFile = UniqueFile FilePath FileID DeviceID FileOffset (Maybe String)
    deriving (Eq, Ord, Show, Data, Typeable)

-- TODO: extract the Word* value out of these and store it into my own unique newtype
deriving instance Data COff -- FileOffset
deriving instance Data CDev -- DeviceID
deriving instance Data CIno -- FileID

-- 1. Load the IxSet of known/processed files from acid-state
-- 2. Process it
--  - Disclaimer this only works for file on the same device id, if there's multiple
--  - file on other devices, the only way to deal with these is to actually hash/1:1
--  - compare the files, in this case hasing is probably adequate.
-- 4. Should have some basic cross device/fs detection, ex we have to process the hash
--    of all files anyway, so might as well do some quick duplication detection and warn
--    user of duplicates/store info on who/which files duplicates which
instance IS.Indexable UniqueFile where
    empty = IS.ixSet
        [ IS.ixGen (IS.Proxy :: IS.Proxy FileID)
        , IS.ixGen (IS.Proxy :: IS.Proxy DeviceID)
        ]

-- IxSet Tuple of (known files, new files)
type SyncSet = (IS.IxSet UniqueFile, IS.IxSet UniqueFile)

initSyncSet :: (IS.IxSet UniqueFile) -> SyncSet
initSyncSet k = (k, IS.empty)

-- Updates the SyncSet
-- 1. If new file, update the (new files) set
-- 2. Otherwise, update the (known files) set
updateSyncSet :: SyncSet -> UniqueFile -> SyncSet
updateSyncSet s u = if (isNewFile s u) then updateNewFile s u else updateKnownFile s u

-- 1. Check to see if (known files) set is empty, if so, return True
-- 2. See if the DeviceID exists, if not, return True
-- 3. See if the FileID exists, if not, return True
-- 4. Otherwise, return False
isNewFile :: SyncSet -> UniqueFile -> Bool
isNewFile (k, n) u@(UniqueFile _ fid did _ _) =
    if IS.null k
    then True
    else if IS.null (IS.getEQ did k)
         then True
         else if IS.null (IS.getEQ fid (IS.getEQ did k))
              then True
              else False

updateKnownFile :: SyncSet -> UniqueFile -> SyncSet
updateKnownFile (k, n) u = ((IS.delete u k), n)

updateNewFile :: SyncSet -> UniqueFile -> SyncSet
updateNewFile (k, n) u = (k, (IS.insert u n))


-- FileSync
-- 1. If file, update SyncSet
-- 2. If directory, call DirectorySync
-- 3. Otherwise, Return original SyncSet
fileSync :: SyncSet -> FilePath -> IO SyncSet
fileSync s p = do
    -- TODO: figure out a good approach for dealing with hard and symbolic links
    fs <- getSymbolicLinkStatus p

    if isRegularFile fs
    then return $ updateSyncSet s $ UniqueFile p (fileID fs) (deviceID fs) (fileSize fs) Nothing
    else if isDirectory fs
         then directorySync s p
         else return s

-- DirectorySync
-- 1. Fold over the directory and running FileSync on each file/directory
-- 2. FileSync will recursivly call DirectorySync on directories
directorySync :: SyncSet -> FilePath -> IO SyncSet
directorySync s p = foldM fileSync s =<< properNames p
    where
        properNames topdir = do
            names <- getDirectoryContents topdir
            return $ map (topdir </>) $ filter (`notElem` [".", ".."]) names

-- FileDirectorySync
-- This folds over the provided list of FilePath
-- Takes a list of filepath, then latter on a IxSet of known file, and returns a
-- SyncSet
fileDirectorySync :: [FilePath] -> IO SyncSet
fileDirectorySync = foldM fileSync (initSyncSet IS.empty)
