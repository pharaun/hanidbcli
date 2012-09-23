{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
import HClient.Sync

import Test.QuickCheck

import Data.Int
import Data.Word
import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString, (</>))
import Prelude hiding (FilePath)
import System.Posix.Types
import qualified Data.IxSet as IS

-- SyncSet = Set UniqueFile, Set UniqueFile
--
-- UniqueFile = FilePath, File Inode, Device Id, File Size, Hash?
--
-- IxSet indexes on File Inode, Device ID
--
-- Disclaimer:
--  This only works for file on the same device id, if there's multiple device ids, only way to
--  know if they are the same file or not is to hash and compare the files
--
--  Need to add basic cross device/fs detection and convoying around the hashes
--
-- Properties: (update known file)
--  The known file must always have its entry removed
--  there must either be one less entry or a empty set
--
-- Properties: (updating syncset)
--  a file added MUST always be either removed from known set or added to new set
--
-- More properties to come (RE hashes)
--

-- TODO: Figure out a good way to generate a fileHash and a filePath
instance Arbitrary UniqueFile where
    arbitrary = do
        filePath <- elements ["foo", "bar", "baz"]
        fileID <- arbitrary
        deviceID <- arbitrary
        fileSize <- arbitrary -- TODO: Positive integers
        fileHash <- elements ["foo", "bar", "baz"]

        return $ UniqueFile
            (decodeString filePath)
            (CIno fileID)
            (CDev deviceID)
            (COff fileSize)
            (Just fileHash)

-- TODO: Make sure we have UniqueFiles that can be: 1) new file, 2) known file
instance Arbitrary (IS.IxSet UniqueFile) where
    arbitrary = do
        files <- arbitrary :: Gen UniqueFile
        return (IS.fromList [files])

instance Arbitrary SyncSet where
    arbitrary = do
        knownFiles <- arbitrary :: Gen UniqueFile
        return (IS.fromList [knownFiles], IS.empty)


-- Properties: (init syncset)
--  an Init SyncSet must always have the "known files" be populated
--  it can be anything from nothing to thousands
prop_emptySyncSet1 = (initSyncSet $ IS.fromList []) == (IS.empty, IS.empty)
prop_emptySyncSet2 = (initSyncSet $ IS.empty) == (IS.empty, IS.empty)
prop_arbitraryKnownSyncSet xs = (initSyncSet xs) == (xs, IS.empty)


-- Properties: (updating new file)
--  the new file must always have its entry added
--  there must be at least 1 entry (the just added entry)
prop_newFileEmptySyncSet uf = (updateNewFile (initSyncSet IS.empty) uf) == (IS.empty, IS.fromList [uf])
prop_newFileSyncSet uf xs = (updateNewFile (initSyncSet xs) uf) == (xs, IS.fromList [uf])


-- Properties: (isNewFile)
--  If known file set is empty, it MUST always return true
--  if device id does not exist in known set, it must always return true
--  if device id does exist in known set it must consult the file id
--  if device id exist and file id does not exist it must return true
--  if device id exists and file id exists it must return false



-- Test driver and stuff
-- Borrowed from the XMonad project
main :: IO ()
main = do
    quickCheck prop_emptySyncSet1
    quickCheck prop_emptySyncSet2

    quickCheck prop_arbitraryKnownSyncSet

    quickCheck prop_newFileEmptySyncSet
    quickCheck prop_newFileSyncSet
