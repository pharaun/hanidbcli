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
            (decodeString filePath)
            (CIno fileID)
            (CDev deviceID)
            (COff fileSize)
            (Just fileHash)

instance Arbitrary (IS.IxSet UniqueFile) where
    arbitrary = do
        files <- listOf1 arbitrary
        return (IS.fromList files)

hexChar :: Gen Char
hexChar = oneof [ elements ['A'..'F'], elements ['0'..'9'] ]

hexHash :: Gen String
hexHash = vectorOf 32 hexChar

unknownDeviceUnknownFile :: UniqueFile -> Gen UniqueFile
unknownDeviceUnknownFile uf = suchThat arbitrary (deviceOnly uf)
    where
        deviceOnly :: UniqueFile -> UniqueFile -> Bool
        deviceOnly (UniqueFile _ a b _ _) (UniqueFile _ x y _ _) = a /= x && b /= y

knownDeviceFile :: UniqueFile -> UniqueFile -> UniqueFile
knownDeviceFile uf1 uf2 = fileOnly uf1 uf2
    where
        fileOnly :: UniqueFile -> UniqueFile -> UniqueFile
        fileOnly (UniqueFile _ a b _ _) (UniqueFile x _ _ y z) = UniqueFile x a b y z

knownDeviceUnknownFile :: UniqueFile -> Gen UniqueFile
knownDeviceUnknownFile uf = matchDevice uf <$> suchThat arbitrary (unMatchFile uf)
    where
        unMatchFile :: UniqueFile -> UniqueFile -> Bool
        unMatchFile (UniqueFile _ a _ _ _) (UniqueFile _ x _ _ _) = a /= x

        matchDevice :: UniqueFile -> UniqueFile -> UniqueFile
        matchDevice (UniqueFile _ _ a _ _) (UniqueFile w x _ y z) = UniqueFile w x a y z


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
--  Adding a "Hardlink" (same file different filename) must result with both file in SyncSet (?)


-- Properties: (isNewFile)
--  If known file set is empty, it MUST always return true
prop_isNewFile_Empty_SyncSet uf = (isNewFile (initSyncSet IS.empty) uf) == True

--  if device id does not exist in known set, it must always return true
prop_isNewFile_unknownDeviceUnknownFile_SyncSet uf = do
    knduf <- unknownDeviceUnknownFile uf
    return (isNewFile (initSyncSet $ IS.fromList [knduf]) uf == True)

--  if device id exist and file id does not exist it must return true
prop_isNewFile_knownDeviceUnknownFile_SyncSet uf = do
    kdf <- knownDeviceUnknownFile uf
    return (isNewFile (initSyncSet $ IS.fromList [kdf]) uf == True)

--  if device id exists and file id exists it must return false (wrong?)
prop_isNewFile_knownDeviceKnownFile_SyncSet uf1 uf2 =
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


-- Test driver and stuff
-- Borrowed from the XMonad project
main :: IO ()
main = do
    quickCheck prop_empty1_SyncSet
    quickCheck prop_empty2_SyncSet
    quickCheck prop_arbitraryKnown_SyncSet

    quickCheck prop_newFile_Empty_SyncSet
    quickCheck prop_newFile_SyncSet

    quickCheck prop_isNewFile_Empty_SyncSet
    quickCheck prop_isNewFile_unknownDeviceUnknownFile_SyncSet
    quickCheck prop_isNewFile_knownDeviceUnknownFile_SyncSet
    quickCheck prop_isNewFile_knownDeviceKnownFile_SyncSet
