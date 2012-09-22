{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module HClient.Sync
    ( fileDirectorySync
    , UniqueFile(..)
    ) where

import Control.Monad (foldM)
import Data.Data (Data, Typeable)
import Prelude hiding (FilePath)
import qualified Data.IxSet as IS
import qualified System.IO as FP

-- Conduit
import Data.Conduit (($$))
import Data.Conduit.Filesystem (traverse)
import Filesystem (listDirectory)
import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString, (</>))
import qualified Data.Conduit.List as CL

import System.Posix.Files (getSymbolicLinkStatus, isRegularFile, isDirectory, deviceID, fileID, fileSize, FileStatus)
import System.Posix.Types

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


-- Takes a list of FP.FilePath and convert it to real FilePath then fold over it and
-- send it to directorySync where the real magic happens
fileDirectorySync :: [FP.FilePath] -> IO SyncSet
fileDirectorySync p = foldM directorySync (initSyncSet IS.empty) $ map decodeString p

directorySync :: SyncSet -> FilePath -> IO SyncSet
directorySync s p = getSymbolicLinkStatus (encodeString p) >>= \fs ->
    if isRegularFile fs
    then fileSync s p
    else if isDirectory fs
         then traverse False p $$ CL.foldM fileSync s
         else return $ s

fileSync :: SyncSet -> FilePath -> IO SyncSet
fileSync s p = getSymbolicLinkStatus (encodeString p) >>= \fs ->
    return $ updateSyncSet s $ UniqueFile p (fileID fs) (deviceID fs) (fileSize fs) Nothing
