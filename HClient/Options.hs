{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}
module HClient.Options
    ( Options
    , optionMode
    ) where

import System.Console.CmdArgs

data PathType = Directory | File
    deriving (Show, Data, Typeable, Eq)

data Options = Hash
             { paths :: [FilePath]
             , pathType :: PathType
             }
             | Test
             deriving (Show, Data, Typeable)

hash :: Options
hash = Hash
     { paths = def &= args &= typ "PATH"
     , pathType = enum
         [ Directory &= help "Directory (default)"
         , File &= help "Files"
         ]
     } &= help "Create an Ed2k file hash."

test :: Options
test = Test &= help "Test alternate command"

optionMode :: Mode (CmdArgs Options)
optionMode = cmdArgsMode $ modes [hash, test]
    &= program "hAnidbCli"
    &= summary "hAnidbCli v0.1"
    &= help "This is the prototype of the hAnidbCli client that interfaces to the AniDB and also manages your Anime media files."
