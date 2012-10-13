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
            (decodeString filePath)
            (CIno fileID)
            (CDev deviceID)
            (COff fileSize)
            (Just fileHash)

instance Arbitrary (IS.IxSet UniqueStatus) where
    arbitrary = do
        files <- listOf1 arbitrary
        return $ fromListSyncSet files

instance Arbitrary FilePath where
    arbitrary = do
        filePath <- elements ["/home/foo0", "/tmp/bar.tar.gz", "/long/silly/[file]name.exe.jpg"]
        return (decodeString filePath)

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

sameFileName :: UniqueFile -> UniqueFile -> Bool
sameFileName (UniqueFile a _ _ _ _) (UniqueFile x _ _ _ _) = (a == x)

-- Replace UniqueFile's FilePath Set with provided one
newUniqueFile :: UniqueFile -> FilePath -> UniqueFile
newUniqueFile (UniqueFile _ b c d e) x = UniqueFile x b c d e

-- TypeChecking this away for now
mergedUniqueFile = undefined

-- Generation of custon SyncSet
fromPairsSyncSet :: [(UniqueFile, IxFileStatus)] -> SyncSet
fromPairsSyncSet = IS.fromList . fmap (uncurry ((UniqueStatus .) . (,)))


-- Properties: (init syncset)
--  an Init SyncSet must always have the "known files" be populated
prop_empty1_SyncSet = (fromListSyncSet []) == emptySyncSet
prop_empty2_SyncSet = (emptySyncSet) == IS.empty

--  it can be anything from nothing to thousands
prop_arbitraryKnown_SyncSet xs = (fromListSyncSet xs) == fromListSyncSet xs


-- Properties: (isNewFile)
--  If known file set is empty, it MUST always return true
prop_isNewFile_Empty_SyncSet uf = (isNewFile (emptySyncSet) uf) == True

--  if device id does not exist in known set, it must always return true
prop_isNewFile_unknownDeviceUnknownFile_SyncSet uf1 uf2 =
    not (sameDevice uf1 uf2) ==>
        (isNewFile (fromListSyncSet [uf1]) uf2 == True)

--  if device id exist and file id does not exist it must return true
prop_isNewFile_knownDeviceUnknownFile_SyncSet uf1 mock =
    not (sameFile uf1 mock) ==>
        let uf3 = mergeDeviceID uf1 mock
        in (isNewFile (fromListSyncSet [uf1]) uf3 == True)

--  if device id exists and file id exists, but different filename return true
prop_isNewFile_knownDeviceKnownFile_SyncSet uf1 mock =
    not (sameFileName uf1 mock) ==>
        let kdf = knownDeviceFile uf1 mock
        in (isNewFile (fromListSyncSet [kdf]) uf1 == True)

--  if device, file id and filename is the same, return false
prop_isNewFile_knownDeviceKnownFileKnownName_SyncSet uf = isNewFile (fromListSyncSet [uf]) uf == False


-- Properties: (updating new file)
--  the new file must always have its entry added
prop_newFile_Empty_SyncSet uf = (updateSyncSet (emptySyncSet) uf) == fromPairsSyncSet [(uf, New)]
prop_newFile_SyncSet uf xs =
    (isNewFile (fromListSyncSet xs) uf) ==>
        (updateSyncSet (fromListSyncSet xs) uf) == fromPairsSyncSet ([(uf, New)] ++ [(x, Unseen) | x <- xs])

--  Adding the same file must result in the same SyncSet
prop_newFile_sameFile_SyncSet uf =
    (updateSyncSet (updateSyncSet (emptySyncSet) uf) uf) == fromPairsSyncSet [(uf, New)]

--  Adding a "Hardlink" (same file different filename) must result with both file in the syncset
prop_newFile_hardLinkFile_SyncSet uf1 path =
    let uf2 = newUniqueFile uf1 path
    in (updateSyncSet (updateSyncSet (emptySyncSet) uf1) uf2 == fromPairsSyncSet [(uf1, New), (uf2, New)])

-- TODO
--  Adding the same file to the syncset that is already "known" ... (Does not support this)


-- Properties: (update known file)
--  there must either be one less entry or a empty set
prop_knownFile_alwaysRemove_SyncSet kf1 =
    let ss = fromListSyncSet [kf1]
    in updateSyncSet ss kf1 == fromPairsSyncSet [(kf1, Seen)]

--  The known file must always have its entry removed
--  TODO: FAILABLE
prop_knownFile_alwaysRemoveCorrectFile_SyncSet kf1 kf2 =
    let ss = fromListSyncSet [kf1, kf2]
    in updateSyncSet ss kf2 == fromPairsSyncSet [(kf1, Unseen), (kf2, Seen)]

--  Removing the same file twice must be the same as removing it once
prop_knownFile_removeTwice_SyncSet kf1 =
    let ss = fromListSyncSet [kf1]
    in updateSyncSet (updateSyncSet ss kf1) kf1 == fromPairsSyncSet [(kf1, Seen)]

-- TODO: FAILABLE
prop_knownFile_removeOneFileTwice_SyncSet kf1 kf2 =
    let ss = fromListSyncSet [kf1, kf2]
    in updateSyncSet (updateSyncSet ss kf2) kf2 == fromPairsSyncSet [(kf1, Unseen), (kf2, Seen)]

--  removing a hardlink must remove the correct file
prop_knownFile_removeOneHardLinkFile_SyncSet kf1 mock_path =
    let kdf = mergedUniqueFile kf1 mock_path
        nkdf = newUniqueFile kf1 mock_path
        ss = fromListSyncSet [kdf]
    in updateSyncSet ss kf1 == (fromListSyncSet [nkdf])

--  removing a hardlink twice must only remove the correct file and not the other file
prop_knownFile_removeOneHardLinkFileTwice_SyncSet kf1 mock_path =
    let kdf = mergedUniqueFile kf1 mock_path
        nkdf = newUniqueFile kf1 mock_path
        ss = fromListSyncSet [kdf]
    in updateSyncSet (updateSyncSet ss kf1) kf1 == (fromListSyncSet [nkdf])


-- Properties: (updating syncset)
--  a file added MUST always be either removed from known set or added to new set
-- updateSyncSet :: SyncSet -> UniqueFile -> SyncSet
-- updateSyncSet s u = if isNewFile s u then updateSyncSet s u else updateSyncSet s u


-- Test driver and stuff
-- Borrowed from the XMonad project
main :: IO ()
main = sequence_ testlist
    where
        arg      = stdArgs { maxSuccess=1000 }
        testlist =
            [ myTest "empty SyncSet 1" prop_empty1_SyncSet
            , myTest "empty SyncSet 2" prop_empty2_SyncSet
            , myTest "arbitrary SyncSet" prop_arbitraryKnown_SyncSet

            , myTest "new file" prop_isNewFile_Empty_SyncSet
            , myTest "new file - unknown device & file" prop_isNewFile_unknownDeviceUnknownFile_SyncSet
            , myTest "new file - known device, unknown file" prop_isNewFile_knownDeviceUnknownFile_SyncSet
            , myTest "new file - known device & file" prop_isNewFile_knownDeviceKnownFile_SyncSet
            , myTest "new file - known device & file & name" prop_isNewFile_knownDeviceKnownFileKnownName_SyncSet

            , myTest "add file" prop_newFile_Empty_SyncSet
            , myTest "add file - non-empty SyncSet" prop_newFile_SyncSet
            , myTest "add same file" prop_newFile_sameFile_SyncSet
            , myTest "add hardlinked file" prop_newFile_hardLinkFile_SyncSet

            , myTest "remove file" prop_knownFile_alwaysRemove_SyncSet
            , myTest "remove correct file" prop_knownFile_alwaysRemoveCorrectFile_SyncSet
            , myTest "remove twice" prop_knownFile_removeTwice_SyncSet
            , myTest "remove same file twice" prop_knownFile_removeOneFileTwice_SyncSet

            , myTest "remove hardlink file" prop_knownFile_removeOneHardLinkFile_SyncSet
            , myTest "remove hardlink file twice" prop_knownFile_removeOneHardLinkFileTwice_SyncSet
            ]

        myTest :: Testable a => String -> a -> IO ()
        myTest b a = putStrLn b >> quickCheckWith arg a >> putStrLn ""
