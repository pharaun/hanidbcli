{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module HClient.Options
    ( Options(..)

    , optionMode
    ) where

import System.Console.CmdArgs

-- Basic options list
--
-- Main:
--  Start - Start the network/hasher/management daemon
--  Stop  - Stop the network/hasher/management daemon
--
-- Supporting:
--  Hash - Hash a list of file/directory (of files), and return the hashes.
--         Look into maybe having it check the metadata and check the other hashes?
--         Useful for integrity checks?
--  Sync - Sync the filesystem info to the metadata stored in the database?
--         Also find any new files/directory and add it to the db?
--  Dump - Dump the metadata & cache to disk.
--  Status - Status of what is going on (ie % hashed, % getting info, % renamed, etc)
--
-- File Management:
--  Rename - Rename a set of directory/files - Have some sort of DSL/Config for this?
--           Simplest would be a printf format "{series}/{group}-{epno}-{media}.{fileext}"
--  ToWatch - Place the series under the to_watch directory (need to figure out implementation)
--  Done - Same as above ^
--  List - List Series and their status (watched, new, mylist, etc...), episode, etc..
--
-- MyList: (Dont worry for now)
--  Add - Add to my list
--  Remove - Remove from my list
--  ...
--

data Options = Start
             | Stop { force :: Bool }
             | Hash { hashPaths :: [FilePath] }
             | Sync { syncPaths :: [FilePath] }
             | Dupe { dupePaths :: [FilePath] }
             | Dump { cache :: Bool }
             | Status
             | List
             deriving (Show, Data, Typeable)

start :: Options
start = Start &= help "Start up the service daemon."

stop :: Options
stop = Stop
     { force = def &= help "Force kill the service daemon."
     } &= help "Stop the service daemon."

sync :: Options
sync = Sync
     { syncPaths = def &= args &= typ "PATHS"
     } &= help "Sync the meta-data store with the filesystem."

dupe :: Options
dupe = Dupe
     { dupePaths = def &= args &= typ "PATHS"
     } &= help "Check the meta-data and files on the filesystem for possible duplicates."

dump :: Options
dump = Dump
     { cache = def &= help "Dump the cache also to disk."
     }&= help "Dump the meta-data store to disk."

status :: Options
status = Status &= help "Get the status of all in progress jobs."

list :: Options
list = List &= help "List the series being managed and their status."

hash :: Options
hash = Hash
     { hashPaths = def &= args &= typ "PATHS"
     } &= help "Create one or more Ed2k file hash."

optionMode :: Mode (CmdArgs Options)
optionMode = cmdArgsMode $ modes [start, stop, hash, sync, dupe, dump, status, list]
    &= program "hAnidbCli"
    &= summary "hAnidbCli v0.1"
    &= help "This is the prototype of the hAnidbCli client that interfaces to the AniDB and also manages your Anime media files."
