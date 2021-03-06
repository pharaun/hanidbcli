module HClient.SyncHash
    ( fileDirectorySyncHash
    ) where

import HClient.Sync
import HClient.Hasher

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data, Typeable)
import Data.Maybe (isNothing, catMaybes)
import Prelude hiding (FilePath, catch)
import qualified Data.IxSet as IS
import qualified Data.List as L
import qualified Data.Set as Set
import qualified System.IO as FP

-- Conduit
import Data.Conduit (($$), (=$), ($=), runResourceT, MonadResource, GSource, GInfConduit, awaitForever, yield, GInfSink)
import qualified Data.Conduit.List as CL

--  1. Acquire List of UniqueFile (Empty or acid-state)
--  2. Load list of UniqueFile into a SyncSet with all state value to Unseen
--  3. tranverse through filesystem updating SyncSet
--
--  4. Break SyncSet into ((dnode, inode), [(UniqueFile, Status)])
--  5. Create a conduit source for this ^
--
--  6. Conduit will take a single paired tuple which has ((dnode, inode), [(UniqueFile, Status)])
--      a. Synchronize hash to all unique file if one exists
--      b. pass it on
--      c. Run a hasher on any that needs a hasher to be ran (io conduit)
--      d. pass it on
--      e. (for now dump out)
--      f. Check to see if the hash exists if not, acquire information from network (io conduit)
--      g. pass it on
--      h. anything else that needs to be done (store to disk, displayed, etc) as a sink.
--
--  7. Step (h) is for "configuration", such as depending on network/config we may have further steps
--      needed, or further data that needs to be fetched, each stage can probably be its own conduit
--      also we will need some sinks such as - moving/renaming files, storing to disk, verifying the file
--      etc...

fileDirectorySyncHash :: [FP.FilePath] -> IO String
fileDirectorySyncHash xs = do
    -- SyncSet
    sync <- fileDirectorySync xs
    a <- runResourceT $ (groupedSyncSetSource sync $= cloneHashIfExists $$ generateHashAsNeeded =$ CL.consume)
    return $ show a

-- Custom SyncSet groups Conduit Source
-- TODO: may want to consider exporting the [UniqueStatus] as an IxSet on Status and hash
--groupedSyncSetSource :: Monad m => SyncSet -> GSource m ((DeviceID, FileID), [UniqueStatus])
groupedSyncSetSource :: Monad m => SyncSet -> GSource m [UniqueStatus]
groupedSyncSetSource ss =
    let groups = groupByDeviceAndFile ss
    in CL.sourceList $ map snd groups

-- Clone the hash among the UniqueStatus group if any members has a hash
cloneHashIfExists :: Monad m => GInfConduit [UniqueStatus] m [UniqueStatus]
cloneHashIfExists = awaitForever $ (\i -> do
    let (noHash, hasHash) = L.partition (\h -> isNothing (getHash h)) i

    -- Check if empty
    if L.null hasHash
    then yield noHash
    else let reduced = reduceHash hasHash
        in if (Set.size reduced) == 1
        then yield ((applyHash noHash $ head $ Set.toList reduced) ++ hasHash)
        else yield i -- TODO: Yield an error, something is not right here
    )
    where
        reduceHash :: [UniqueStatus] -> Set.Set String
        reduceHash xs = Set.fromList $ catMaybes $ map getHash xs

-- Apply a generated hash to all UniqueStatus
applyHash :: [UniqueStatus] -> String -> [UniqueStatus]
applyHash xs s = map (\x -> setHash x s) xs

-- Generate a hash for any UniqueFile that does not have a hash value
generateHashAsNeeded :: MonadIO m => GInfConduit [UniqueStatus] m [UniqueStatus]
generateHashAsNeeded = awaitForever $ (\i -> do
    let (noHash, hasHash) = L.partition (\h -> isNothing (getHash h)) i

    -- Check if empty
    if L.null noHash
    then yield hasHash
    else (\x -> yield $ applyHash noHash $ snd $ head x)
        =<< (liftIO $ fileHash $ getFileName $ head noHash) -- TODO: not safe with the head
    )

-- A Sink for dumping it out, probably have couple sinks that we can use such as
-- acid-state, string/dump it out to the console, etc...
