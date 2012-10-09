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
-- UniqueFile = FilePath, File Inode, Device Id, File Size, Hash?
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
-- Properties: (hardlink, symlink)
--  Deal with hardlink and symlink in a safe manner
--  hardlink -> multiple path assocated with a unique file
--  symlink -> (? unique file pointer? or some sort of marker to identify as a symlink)
--      could be done via lstat + stat and using one or the other as the dst vs source
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

-- Take the 2nd UniqueFile and merge in the 1st UniqueFile device and file id
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
prop_empty1_SyncSet = (initSyncSet $ IS.fromList []) == (IS.empty, IS.empty, IS.empty)
prop_empty2_SyncSet = (initSyncSet $ IS.empty) == (IS.empty, IS.empty, IS.empty)

--  it can be anything from nothing to thousands
prop_arbitraryKnown_SyncSet xs = (initSyncSet xs) == (xs, IS.empty, IS.empty)


-- Properties: (isNewFile)
--  If known file set is empty, it MUST always return true
prop_isNewFile_Empty_SyncSet uf = (isNewFile (initSyncSet IS.empty) uf) == True

--  if device id does not exist in known set, it must always return true
prop_isNewFile_unknownDeviceUnknownFile_SyncSet uf1 uf2 =
    not (sameDevice uf1 uf2) ==>
        (isNewFile (initSyncSet $ IS.fromList [uf1]) uf2 == True)

--  if device id exist and file id does not exist it must return true
prop_isNewFile_knownDeviceUnknownFile_SyncSet uf1 mock =
    not (sameFile uf1 mock) ==>
        let uf3 = mergeDeviceID uf1 mock
        in (isNewFile (initSyncSet $ IS.fromList [uf1]) uf3 == True)

--  if device id exists and file id exists it must return false
--  TODO: hardlink dealing with here)
--  How do we want to deal with hardlinks here, IE do we just want to add the hardlink
--  to the known file or treat it like a new file?
prop_isNewFile_knownDeviceKnownFile_SyncSet uf1 mock =
    let kdf = knownDeviceFile uf1 mock
    in (isNewFile (initSyncSet $ IS.fromList [kdf]) uf1 == False)


-- Properties: (updating new file)
--  the new file must always have its entry added
prop_newFile_Empty_SyncSet uf = (updateNewFile (initSyncSet IS.empty) uf) == (IS.empty, IS.fromList [uf], IS.empty)
prop_newFile_SyncSet uf xs = (updateNewFile (initSyncSet xs) uf) == (xs, IS.fromList [uf], IS.empty)

--  Adding the same file must result in the same SyncSet
prop_newFile_sameFile_SyncSet uf =
    (updateNewFile (updateNewFile (initSyncSet IS.empty) uf) uf) == (IS.empty, IS.fromList [uf], IS.empty)

--  Adding a "Hardlink" (same file different filename) must result with both file merged in the SyncSet
prop_newFile_hardLinkFile_SyncSet uf path =
    let nhlf = mergedUniqueFile uf path
        uf2  = newUniqueFile uf path
    in (updateNewFile (updateNewFile (initSyncSet IS.empty) uf) uf2 == (IS.empty, IS.fromList [nhlf], IS.empty))

--  Adding a duplicate filename (hardlink) to a pre-existing new unique file in syncset
prop_newFile_duplicateHardLinkFile_SyncSet uf path =
    let nhlf = mergedUniqueFile uf path
        uf2  = newUniqueFile uf path
    in (updateNewFile (updateNewFile (initSyncSet IS.empty) nhlf) uf2 == (IS.empty, IS.fromList [nhlf], IS.empty))


-- Properties: (update known file)
--  Removing a file from a empty syncset does not explode things
prop_knownFile_emptyRemove kf1 =
    let ss = initSyncSet $ IS.empty
    in updateKnownFile ss kf1 == ss

--  there must either be one less entry or a empty set
prop_knownFile_alwaysRemove_SyncSet kf1 =
    let ss = initSyncSet $ IS.fromList [kf1]
    in updateKnownFile ss kf1 == (initSyncSet IS.empty)

--  The known file must always have its entry removed
prop_knownFile_alwaysRemoveCorrectFile_SyncSet kf1 kf2 =
    let ss = initSyncSet $ IS.fromList [kf1, kf2]
    in updateKnownFile ss kf2 == (initSyncSet $ IS.fromList [kf1])

--  Removing the same file twice must be the same as removing it once
prop_knownFile_removeTwice_SyncSet kf1 =
    let ss = initSyncSet $ IS.fromList [kf1]
    in updateKnownFile (updateKnownFile ss kf1) kf1 == (initSyncSet IS.empty)

prop_knownFile_removeOneFileTwice_SyncSet kf1 kf2 =
    let ss = initSyncSet $ IS.fromList [kf1, kf2]
    in updateKnownFile (updateKnownFile ss kf2) kf2 == (initSyncSet $ IS.fromList [kf1])

--  removing a hardlink must remove the correct file
prop_knownFile_removeOneHardLinkFile_SyncSet kf1 mock_path =
    let kdf = mergedUniqueFile kf1 mock_path
        nkdf = newUniqueFile kf1 mock_path
        ss = initSyncSet $ IS.fromList [kdf]
    in updateKnownFile ss kf1 == (initSyncSet $ IS.fromList [nkdf])

--  removing a hardlink twice must only remove the correct file and not the other file
prop_knownFile_removeOneHardLinkFileTwice_SyncSet kf1 mock_path =
    let kdf = mergedUniqueFile kf1 mock_path
        nkdf = newUniqueFile kf1 mock_path
        ss = initSyncSet $ IS.fromList [kdf]
    in updateKnownFile (updateKnownFile ss kf1) kf1 == (initSyncSet $ IS.fromList [nkdf])


-- Properties: (updating syncset)
--  a file added MUST always be either removed from known set or added to new set
-- updateSyncSet :: SyncSet -> UniqueFile -> SyncSet
-- updateSyncSet s u = if isNewFile s u then updateNewFile s u else updateKnownFile s u


-- Test driver and stuff
-- Borrowed from the XMonad project
main :: IO ()
main = sequence_ testlist
    where
        arg      = stdArgs { maxSuccess=200 }
        testlist =
            [ myTest "empty SyncSet 1" prop_empty1_SyncSet
            , myTest "empty SyncSet 2" prop_empty2_SyncSet
            , myTest "arbitrary SyncSet" prop_arbitraryKnown_SyncSet

            , myTest "new file" prop_isNewFile_Empty_SyncSet
            , myTest "new file - unknown device & file" prop_isNewFile_unknownDeviceUnknownFile_SyncSet
            , myTest "new file - known device, unknown file" prop_isNewFile_knownDeviceUnknownFile_SyncSet
            , myTest "new file - known device & file" prop_isNewFile_knownDeviceKnownFile_SyncSet
            -- TODO: Add new file hardlink handling here

            , myTest "add file" prop_newFile_Empty_SyncSet
            , myTest "add file - non-empty SyncSet" prop_newFile_SyncSet
            , myTest "add same file" prop_newFile_sameFile_SyncSet
            , myTest "add hardlinked file" prop_newFile_hardLinkFile_SyncSet
            , myTest "add duplicate hardlinked file" prop_newFile_duplicateHardLinkFile_SyncSet

            , myTest "remove file from empty set" prop_knownFile_emptyRemove
            , myTest "remove file" prop_knownFile_alwaysRemove_SyncSet
            , myTest "remove correct file" prop_knownFile_alwaysRemoveCorrectFile_SyncSet
            , myTest "remove twice" prop_knownFile_removeTwice_SyncSet
            , myTest "remove same file twice" prop_knownFile_removeOneFileTwice_SyncSet

            , myTest "remove hardlink file" prop_knownFile_removeOneHardLinkFile_SyncSet
            , myTest "remove hardlink file twice" prop_knownFile_removeOneHardLinkFileTwice_SyncSet
            ]

        myTest :: Testable a => String -> a -> IO ()
        myTest b a = putStrLn b >> quickCheckWith arg a >> putStrLn ""
