{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module HClient.Sync
    ( fileDirectorySync
    , UniqueFile(..)
    , UniqueStatus(..)
    , IxFileStatus(..)

    -- SyncSet support
    , SyncSet
    , emptySyncSet
    , fromListSyncSet

    , updateSyncSet
    , isNewFile
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

-- "Posix Unique File" identifier, which also holds the file size and a Maybe Hash of the file
-- content for interacting with AniDB and cross device comparsion.
--
-- This does not deal with symbolic links, this will be probably dealt with in a latter iteration
-- in its own data structure/etc. But for now all code ignores symbolic links.
--
-- We indirectly support hardlinks via the container (IxSet) but we don't support hardlinks here
-- in the UniqueFile because of complicatedness of storing/updating the status of the hardlinks.
data UniqueFile = UniqueFile FilePath FileID DeviceID FileOffset (Maybe String)
    deriving (Eq, Ord, Show, Data, Typeable)

-- TODO: The more proper thing to do here is to have my own type/opaque type for dealing with these
-- values
deriving instance Data COff -- FileOffset
deriving instance Data CDev -- DeviceID
deriving instance Data CIno -- FileID

-- File Status, this is used to mark off what file has been seen and what is new, so we can detect
-- the removed files.
data IxFileStatus = Unseen | Seen | New
    deriving (Eq, Ord, Show, Data, Typeable)

-- newtype of UniqueFile and IxFileStatus
newtype UniqueStatus = UniqueStatus (UniqueFile, IxFileStatus)
    deriving (Eq, Ord, Show, Data, Typeable)

getDeviceFileId :: UniqueStatus -> (DeviceID, FileID)
getDeviceFileId (UniqueStatus ((UniqueFile _ fid did _ _), _)) = (did, fid)

getFileName :: UniqueStatus -> FilePath
getFileName (UniqueStatus ((UniqueFile fn _ _ _ _), _)) = fn

getStatus :: UniqueStatus -> IxFileStatus
getStatus (UniqueStatus (_, s)) = s

-- IxSet of the UniqueFiles, there is a couple of rules/indexing decision here to enable certain
-- usage to make things simpler in the long run.
--
-- Query Design:
--  - GroupBy (DeviceID, FileID) - Hardlink groups (Only on same filesystem)
--  - Update File Status - Find via "filename", confirm/match DeviceID+FileID then update status?
--  - GroupBy Status
--
--  IxSet (DeviceID, FileID), (FileName), (Status)
instance IS.Indexable UniqueStatus where
    empty = IS.ixSet
        [ IS.ixFun $ \p -> [ getDeviceFileId p ]
        , IS.ixFun $ \p -> [ getFileName p ]
        , IS.ixFun $ \p -> [ getStatus p ]
        ]

-- The IxSet... type alias
type SyncSet = IS.IxSet UniqueStatus

emptySyncSet :: SyncSet
emptySyncSet = IS.empty

-- Take a list of UniqueFile and initalizes it into a SyncSet with all of the status set to Unseen
fromListSyncSet :: [UniqueFile] -> SyncSet
fromListSyncSet = IS.fromList . map (UniqueStatus . flip (,) Unseen)

-- Updates the SyncSet
-- 1. If new file/hardlink, insert into the SyncSet as New
-- 2. Otherwise, update the relevant file in the SyncSet to Seen
updateSyncSet :: SyncSet -> UniqueFile -> SyncSet
updateSyncSet s u@(UniqueFile fn fid did _ _) =
    let setFile = IS.getOne (IS.getEQ fn (IS.getEQ (did, fid) s))
    in case setFile of
        Nothing -> addNewFile s u
        Just x  -> updateStatus s x
    where
        addNewFile :: SyncSet -> UniqueFile -> SyncSet
        addNewFile s u = IS.insert (UniqueStatus (u, New)) s

        -- Double check the status, make sure its not a New, if so ignore
        updateStatus :: SyncSet -> UniqueStatus -> SyncSet
        updateStatus sx x@(UniqueStatus (u, s))
            | s == New  = sx
            | otherwise = IS.insert (UniqueStatus (u, Seen)) (IS.delete x sx)


-- Check to see if this UniqueFile is new to this SyncSet
-- 1. Query to see if the (FileID, DeviceID) exists, if not, return True
-- 2. Query the subset to see if the FileName exists, if not, return True
-- 3. Return False
--
-- TODO: Normalize the FilePath perhaps to help with indexing and matching on file path/name
isNewFile :: SyncSet -> UniqueFile -> Bool
isNewFile s u@(UniqueFile fn fid did _ _) =
    IS.null s ||
        (IS.null (IS.getEQ (did, fid) s)) ||
            (IS.null (IS.getEQ fn (IS.getEQ (did, fid) s)))


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



-- Takes a list of FP.FilePath and convert it to real FilePath then fold over it and
-- send it to directorySync where the real magic happens
fileDirectorySync :: [FP.FilePath] -> IO SyncSet
fileDirectorySync p = foldM directorySync (fromListSyncSet []) $ map decodeString p

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
    return $ updateSyncSet s $ UniqueFile p (fileID fs) (deviceID fs) (fileSize fs) Nothing
