{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module HClient.Sync
    ( fileDirectorySync
    , UniqueFile(..)

    -- SyncSet support
    , SyncSet
    , initSyncSet
    , updateSyncSet
    -- TODO: Add merging syncset support? Merging UniqueFile?
    , isNewFile
    , updateKnownFile
    , updateNewFile
    ) where

import Control.Monad (foldM)
import Data.Data (Data, Typeable)
import Prelude hiding (FilePath, catch)
import qualified Data.IxSet as IS
import qualified Data.Set as Set
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
--
-- This does not deal with symbolic link, for now the full code base here and the hasher
-- does not directly deal with symbolic links at all, IE if the file under inspection is
-- a symbolic link its ignored.
--
-- Now we do support hardlinks because it would be somewhat deadly to not directly deal with
-- hardlinks here considering we are dependent on Inodes and device ids to identify new/old files.
data UniqueFile = UniqueFile (Set.Set FilePath) FileID DeviceID FileOffset (Maybe String)
    deriving (Eq, Ord, Show, Data, Typeable)

-- TODO: The more proper thing to do here is to have my own type/opaque type for dealing with these
-- values
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

-- IxSet Tuple of (known files, new files, new hardlink to known file)
type SyncSet = (IS.IxSet UniqueFile, IS.IxSet UniqueFile, IS.IxSet UniqueFile)

initSyncSet :: IS.IxSet UniqueFile -> SyncSet
initSyncSet k = (k, IS.empty, IS.empty)

-- Updates the SyncSet
-- 1. If new file, update the (new files) set
-- 2. Otherwise, update the (known files) set
updateSyncSet :: SyncSet -> UniqueFile -> SyncSet
updateSyncSet s u = if isNewFile s u then updateNewFile s u else updateKnownFile s u

-- 1. Check to see if (known files) set is empty, if so, return True
-- 2. See if the DeviceID exists, if not, return True
-- 3. See if the FileID exists, if not, return True
-- 4. Otherwise, return False
-- Does not take in accord hardlinks but for our usecase its only known file that cares
isNewFile :: SyncSet -> UniqueFile -> Bool
isNewFile (k, _, _) u@(UniqueFile _ fid did _ _) =
    IS.null k ||
        (IS.null (IS.getEQ did k) ||
            IS.null (IS.getEQ fid (IS.getEQ did k)))

-- TODO: deal with hardlinks
updateKnownFile :: SyncSet -> UniqueFile -> SyncSet
updateKnownFile (k, n, h) u = (IS.delete u k, n, h)

updateNewFile :: SyncSet -> UniqueFile -> SyncSet
updateNewFile (k, n, h) u = (k, (mergeHardlinkFile n u), h)

-- Find a file in the IxSet and merge in the new hardlinked file
mergeHardlinkFile :: IS.IxSet UniqueFile -> UniqueFile -> IS.IxSet UniqueFile
mergeHardlinkFile s u@(UniqueFile _ fid did _ _) =
    let setFile = IS.getOne (IS.getEQ fid (IS.getEQ did s))
    in case setFile of
        Nothing -> IS.insert u s
--        Just x  -> IS.updateIx (fid, did) (mergeUniqueFile x u) s
        Just x  -> IS.insert (mergeUniqueFile x u) (IS.delete x s)
    where
        mergeUniqueFile :: UniqueFile -> UniqueFile -> UniqueFile
        mergeUniqueFile (UniqueFile a b c d e) (UniqueFile x _ _ _ _) = UniqueFile (Set.union a x) b c d e

-- Takes a list of FP.FilePath and convert it to real FilePath then fold over it and
-- send it to directorySync where the real magic happens
fileDirectorySync :: [FP.FilePath] -> IO SyncSet
fileDirectorySync p = foldM directorySync (initSyncSet IS.empty) $ map decodeString p

-- TODO: deal with file access, permission, symlink, hardlinks, etc
directorySync :: SyncSet -> FilePath -> IO SyncSet
directorySync s p = getSymbolicLinkStatus (encodeString p) >>= \fs ->
    if isRegularFile fs
    then fileSync s p
    else if isDirectory fs
         -- TODO: deal with too many level of symbolic links (Ignore symbolic links for now)
         then traverse False (\_ -> True) p $$ CL.foldM fileSync s
         else return s

fileSync :: SyncSet -> FilePath -> IO SyncSet
fileSync s p = getSymbolicLinkStatus (encodeString p) >>= \fs ->
    return $ updateSyncSet s $ UniqueFile (Set.fromList [p]) (fileID fs) (deviceID fs) (fileSize fs) Nothing
