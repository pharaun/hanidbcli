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

import Data.Maybe (isNothing, fromJust)
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


-- Looks like its time to reframe the design of the SyncSet
-- We tried to be clever and grow and remove items from the SyncSet, this does not work, so
-- Perhaps its time for a respin on the SyncSet....
--
-- IxSet on device and file id seems to work good for the general lookup case.
--  HardLink on UniqueFiles seems to be painful because of nested Set, better functions
--      for dealing with this would go a long way toward reducing the pain.
--
--  Perhaps take advantage of IxSet and add a "status" type like (Seen, New, Unseen) and
--      default to Unseen, which when you query on Unseen it will yield a list of all Unseen
--      files and hardlink (assuming one uniquefile per link/file)
--      It would also yield up a list of all new file, then anything still marked unseen as deleted
--
--  However what's the best manner to group up each hardlinks? (We have GroupBy which is on a key...)
--      Perhaps a unified index of device+file could work but would need to be *careful*
--
--  Perhaps it may be worth it to break up the data into a single IxSet for device, then for each
--      device it gets a subIxSet for the fileId that way all of the groupBy/etc function would work
--      However it would still not allow the modify op because of multiple file to a fileid...
--
--  Even (Device, File) set as a index would not be global unique in case of hardlink, unless we
--      embed the hardlink info into the UniqueFile plus their status info...
--
--  I am starting to think that there may be a benefit of treating even hardlinks as its own file cos
--      of the file pointer, as long as we do not *alter* the file at all (just rename the file pointer)
--      then for all intents and purpose a hardlink is "its own" file, it just has same inode/device for
--      the hashing purpose and for notification purpose of "duplicate files"
--
--  So In ideal case you would not have hardlinks (symlinks perhaps, but that's a thronnier problem
--      for latter anyway)

-- New Structure.
--
-- IxSet (deviceId) of IxSet (FileID) of (UniqueFile, Status), where Status = (Unseen, Seen, New)
--  Would probably want index on file id, then file name, for fast discovery for updating file status
--  and detection of new/seen files
--
-- Query Design:
--  - GroupBy (DeviceID, FileID) - Hardlink groups
--  - Update File Status - Find via "filename", confirm/match DeviceID+FileID then update status?
--  - GroupBy Status
--
--  IxSet (DeviceID, FileID), (FileName), (Status)



--
-- We have a UniqueFile in the IxSet that may have one or more "hardlinks"
-- We also have a UniqueFile (new?) that may have one or more "hardlinks"
--
-- We need to determite if its all entirely a new hardlinked file, if so add it to
-- new hardlink to known file.
--
-- Otherwise remove the relevant hardlink from the IxSet or the whole UniqueFile if it was
-- the whole thing.
--
-- 1. Detect what is new, store the subset that is new into "new hardlink IxSet"
-- 2. Remove UniqueFile out of IxSet
--  a. Remove known file out, if any left, reinsert into IxSet "known"
--  b. Otherwise leave it out?
updateKnownFile :: SyncSet -> UniqueFile -> SyncSet
updateKnownFile (k, n, h) u@(UniqueFile _ fid did _ _) =
    let setFile = IS.getOne (IS.getEQ fid (IS.getEQ did k))
    in case setFile of
        Nothing -> (k, n, (mergeHardlinkFile h u))
        Just x  -> ((removeHardlinkFile k x u), n, (addHardlinkFile k x u))

-- Find the old hardlink and remove it, either remove the whole UniqueFile or part of it
removeHardlinkFile :: IS.IxSet UniqueFile -> UniqueFile -> UniqueFile -> IS.IxSet UniqueFile
removeHardlinkFile s u@(UniqueFile a b c d e) (UniqueFile x _ _ _ _) =
    let keep = Set.difference a (Set.intersection a x)
    in  if Set.null keep
        then IS.delete u s
        else IS.insert (UniqueFile keep b c d e) (IS.delete u s)

-- Finds the new hardlink then merge it into the new hardlink set
addHardlinkFile :: IS.IxSet UniqueFile -> UniqueFile -> UniqueFile -> IS.IxSet UniqueFile
addHardlinkFile s (UniqueFile a b c d e) (UniqueFile x _ _ _ _) =
    let new = Set.difference x (Set.intersection x a)
    in  if Set.null new
        then s
        else mergeHardlinkFile s (UniqueFile new b c d e)


updateNewFile :: SyncSet -> UniqueFile -> SyncSet
updateNewFile (k, n, h) u = (k, (mergeHardlinkFile n u), h)

-- Find a file in the IxSet and merge in the new hardlinked file
mergeHardlinkFile :: IS.IxSet UniqueFile -> UniqueFile -> IS.IxSet UniqueFile
mergeHardlinkFile s u@(UniqueFile _ fid did _ _) =
    let setFile = IS.getOne (IS.getEQ fid (IS.getEQ did s))
    in case setFile of
        Nothing -> IS.insert u s
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
