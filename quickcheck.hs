{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
import HClient.Sync

import Test.QuickCheck

import Control.Applicative ((<$>))
import Data.Int
import Data.Word
import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString, (</>))
import Prelude hiding (FilePath)
import System.Posix.Types
import qualified Data.IxSet as IS
import qualified Data.Set as Set

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
-- More properties to come (RE hashes)
--

instance Arbitrary UniqueFile where
    arbitrary = do
        filePath <- elements ["/home/foo", "/tmp/bar.gz", "/long/silly/[file]name(terrible).exe.jpg"]
        fileID <- arbitrary :: Gen Word64
        deviceID <- arbitrary :: Gen Word64
        Positive fileSize <- arbitrary
        fileHash <- hexHash

        return $ UniqueFile
            -- TODO: find a way to generate file path, maybe lists for hardlinks
            (Set.fromList [decodeString filePath])
            (CIno fileID)
            (CDev deviceID)
            (COff fileSize)
            (Just fileHash)

instance Arbitrary (IS.IxSet UniqueFile) where
    arbitrary = do
        files <- listOf1 arbitrary
        return (IS.fromList files)

instance Arbitrary (Set.Set FilePath) where
    arbitrary = do
        filePath <- elements ["/home/foo0", "/tmp/bar.tar.gz", "/long/silly/[file]name.exe.jpg"]
        return (Set.fromList [decodeString filePath])

hexChar :: Gen Char
hexChar = oneof [ elements ['A'..'F'], elements ['0'..'9'] ]

hexHash :: Gen String
hexHash = vectorOf 32 hexChar


knownDeviceFile :: UniqueFile -> UniqueFile -> UniqueFile
knownDeviceFile uf1 uf2 = fileOnly uf1 uf2
    where
        fileOnly :: UniqueFile -> UniqueFile -> UniqueFile
        fileOnly (UniqueFile _ a b _ _) (UniqueFile x _ _ y z) = UniqueFile x a b y z

-- Merge in a UniqueFile to use the same device id
mergeDeviceID :: UniqueFile -> UniqueFile -> UniqueFile
mergeDeviceID (UniqueFile _ _ a _ _) (UniqueFile w x _ y z) = UniqueFile w x a y z

-- Reject if same device and same file
sameDevice :: UniqueFile -> UniqueFile -> Bool
sameDevice (UniqueFile _ _ b _ _) (UniqueFile _ _ y _ _) = b == y

-- Reject if same device and same file
sameFile :: UniqueFile -> UniqueFile -> Bool
sameFile (UniqueFile _ a _ _ _) (UniqueFile _ x _ _ _) = a == x

-- Merge in a UniqueFile with a provided Set of FilePaths
mergedUniqueFile :: UniqueFile -> (Set.Set FilePath) -> UniqueFile
mergedUniqueFile (UniqueFile a b c d e) x = UniqueFile (Set.union a x) b c d e

-- Replace UniqueFile's FilePath Set with provided one
newUniqueFile :: UniqueFile -> (Set.Set FilePath) -> UniqueFile
newUniqueFile (UniqueFile _ b c d e) x = UniqueFile x b c d e


-- Properties: (init syncset)
--  an Init SyncSet must always have the "known files" be populated
prop_empty1_SyncSet = (initSyncSet $ IS.fromList []) == (IS.empty, IS.empty)
prop_empty2_SyncSet = (initSyncSet $ IS.empty) == (IS.empty, IS.empty)

--  it can be anything from nothing to thousands
prop_arbitraryKnown_SyncSet xs = (initSyncSet xs) == (xs, IS.empty)


-- Properties: (updating new file)
--  the new file must always have its entry added
prop_newFile_Empty_SyncSet uf = (updateNewFile (initSyncSet IS.empty) uf) == (IS.empty, IS.fromList [uf])
prop_newFile_SyncSet uf xs = (updateNewFile (initSyncSet xs) uf) == (xs, IS.fromList [uf])

--  Adding the same file must result in the same SyncSet
prop_newFile_sameFile_SyncSet uf =
    (updateNewFile (updateNewFile (initSyncSet IS.empty) uf) uf) == (IS.empty, IS.fromList [uf])

--  Adding a "Hardlink" (same file different filename) must result with both file in SyncSet
prop_newFile_hardLinkFile_SyncSet uf path =
    let nhlf = mergedUniqueFile uf path
        uf2  = newUniqueFile uf path
    in (updateNewFile (updateNewFile (initSyncSet IS.empty) uf) uf2 == (IS.empty, IS.fromList [nhlf]))


-- Properties: (isNewFile)
--  If known file set is empty, it MUST always return true
prop_isNewFile_Empty_SyncSet uf = (isNewFile (initSyncSet IS.empty) uf) == True

--  if device id does not exist in known set, it must always return true
prop_isNewFile_unknownDeviceUnknownFile_SyncSet uf1 uf2 =
    not (sameDevice uf1 uf2) ==>
        (isNewFile (initSyncSet $ IS.fromList [uf1]) uf2 == True)

--  if device id exist and file id does not exist it must return true
prop_isNewFile_knownDeviceUnknownFile_SyncSet uf1 uf2 = -- TODO: probably better as a newtype on deviceID
    not (sameFile uf1 uf2) ==>
        let uf3 = mergeDeviceID uf1 uf2
        in (isNewFile (initSyncSet $ IS.fromList [uf1]) uf3 == True)

--  if device id exists and file id exists it must return false (wrong?)
prop_isNewFile_knownDeviceKnownFile_SyncSet uf1 uf2 = -- TODO: probably better as a newtype on fileID
    let kdf = knownDeviceFile uf1 uf2
    in (isNewFile (initSyncSet $ IS.fromList [kdf]) uf1 == False)


-- Properties: (update known file)
--  The known file must always have its entry removed
--  there must either be one less entry or a empty set
--  Removing the same file twice must be the same as removing it once
--  removing a hardlink must remove the correct file
--  removing a hardlink twice must only remove the correct file and not the other file


-- Properties: (updating syncset)
--  a file added MUST always be either removed from known set or added to new set

-- Properties: (hardlink, symlink)
--  Deal with hardlink and symlink in a safe manner
--  hardlink -> multiple path assocated with a unique file
--  symlink -> (? unique file pointer? or some sort of marker to identify as a symlink)
--      could be done via lstat + stat and using one or the other as the dst vs source


-- Test driver and stuff
-- Borrowed from the XMonad project
main :: IO ()
main = do
    quickCheck prop_empty1_SyncSet
    quickCheck prop_empty2_SyncSet
    quickCheck prop_arbitraryKnown_SyncSet

    quickCheck prop_newFile_Empty_SyncSet
    quickCheck prop_newFile_SyncSet
    quickCheck prop_newFile_sameFile_SyncSet
    quickCheck prop_newFile_hardLinkFile_SyncSet

    quickCheck prop_isNewFile_Empty_SyncSet
    quickCheck prop_isNewFile_unknownDeviceUnknownFile_SyncSet
    quickCheck prop_isNewFile_knownDeviceUnknownFile_SyncSet
    quickCheck prop_isNewFile_knownDeviceKnownFile_SyncSet
