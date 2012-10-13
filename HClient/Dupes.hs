module HClient.Dupes
    ( detectDupes
    ) where

import HClient.Sync

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

detectDupes :: SyncSet -> IO String
detectDupes = undefined
