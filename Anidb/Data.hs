module Anidb.Data
    ( AnimeArg(..)
    , anime

    , animeDesc
    , calendar
    , character
    , creator

    , EpisodeArg(..)
    , episode

    , FileArg(..)
    , file

    , GroupArg(..)
    , group

    , groupStatus
    ) where

import qualified Anidb.Network as AN
import qualified Anidb.Parse.Reply as APR
import qualified Anidb.Parse.Data as APD

-- Type def
data GroupArg = GroupName String | GroupID Integer

data EpisodeArg = EpisodeID Integer
                | EpisodeAnimeID Integer Integer
                | EpisodeAnimeName String Integer

data AnimeArg = AnimeID Integer | AnimeName String

type AnimeMask = String

type FileAnimeMask = String

type FileMask = String

data FileArg = FileID Integer
             | FileHash String Integer
             | FileAnimeGroup AnimeArg GroupArg Integer


-- Deals with the creator data type/response
creator :: AN.AniNetState -> Integer -> IO APR.AniReplyParsed
creator netState cid = do
    reply <- AN.getData netState "CREATOR" [AN.CreatorID cid]
    return reply

-- TODO: get confirmation on the syntax of the episode list
-- Deals with the character data type/response
character :: AN.AniNetState -> Integer -> IO APR.AniReplyParsed
character netState cid = do
    reply <- AN.getData netState "CHARACTER" [AN.CharacterID cid]
    return reply

-- Calendar
calendar :: AN.AniNetState -> IO APR.AniReplyParsed
calendar netState = do
    reply <- AN.getData netState "CALENDAR" []
    return reply

animeDesc :: AN.AniNetState -> Integer -> IO APR.AniReplyParsed
animeDesc netState aid = do
    reply <- AN.getData netState "ANIMEDESC" [AN.AnimeID aid, AN.DescPart 0]
    -- TODO: Add more logic to fetch all parts and merge em here, otherwise return
    return reply

group :: AN.AniNetState -> GroupArg -> IO APR.AniReplyParsed
group netState (GroupName x) = AN.getData netState "GROUP" [AN.GroupName x]
group netState (GroupID x)   = AN.getData netState "GROUP" [AN.GroupID x]


-- TODO: get an actual sample of the syntax for the ep list
groupStatus :: AN.AniNetState -> Integer -> Maybe Integer -> IO APR.AniReplyParsed
groupStatus netState aid Nothing      = AN.getData netState "GROUPSTATUS" [AN.AnimeID aid]
groupStatus netState aid (Just state) = AN.getData netState "GROUPSTATUS" [AN.AnimeID aid, AN.AnimeCompletionState state]


episode :: AN.AniNetState -> EpisodeArg -> IO APR.AniReplyParsed
episode netState (EpisodeID eid)               = AN.getData netState "EPISODE" [AN.EpisodeID eid]
episode netState (EpisodeAnimeID aid epno)     = AN.getData netState "EPISODE" [AN.AnimeID aid, AN.EpisodeNO epno]
episode netState (EpisodeAnimeName aname epno) = AN.getData netState "EPISODE" [AN.AnimeName aname, AN.EpisodeNO epno]


-- TODO: do more in depth analysis of the mask
anime :: AN.AniNetState -> AnimeArg -> Maybe AnimeMask -> IO APR.AniReplyParsed
anime netState (AnimeID aid) Nothing         = AN.getData netState "ANIME" [AN.AnimeID aid]
anime netState (AnimeID aid) (Just mask)     = AN.getData netState "ANIME" [AN.AnimeID aid, AN.AnimeMask mask]
anime netState (AnimeName aname) Nothing     = AN.getData netState "ANIME" [AN.AnimeName aname]
anime netState (AnimeName aname) (Just mask) = AN.getData netState "ANIME" [AN.AnimeName aname, AN.AnimeMask mask]


-- TODO: Need an idea of what the default anime/file mask is here, also
-- need a short anime mask here
file :: AN.AniNetState -> FileArg -> FileMask -> FileAnimeMask -> IO APR.AniReplyParsed
file netState (FileID fid) fmask amask                      = AN.getData netState "FILE" [AN.FileID fid, AN.FileMask fmask, AN.AnimeMask amask]
file netState (FileHash hash size) fmask amask              = AN.getData netState "FILE" [AN.FileHash hash, AN.FileSize size, AN.FileMask fmask, AN.AnimeMask amask]
file netState (FileAnimeGroup anime group epno) fmask amask = subFile netState anime group epno fmask amask
    where
        subFile :: AN.AniNetState -> AnimeArg -> GroupArg -> Integer -> FileMask -> AnimeMask -> IO APR.AniReplyParsed
        subFile netState (AnimeID aid) (GroupID gid) epno fmask amask         = AN.getData netState "FILE" [AN.AnimeID aid, AN.GroupID gid, AN.EpisodeNO epno, AN.FileMask fmask, AN.AnimeMask amask]
        subFile netState (AnimeID aid) (GroupName gname) epno fmask amask     = AN.getData netState "FILE" [AN.AnimeID aid, AN.GroupName gname, AN.EpisodeNO epno, AN.FileMask fmask, AN.AnimeMask amask]
        subFile netState (AnimeName aname) (GroupID gid) epno fmask amask     = AN.getData netState "FILE" [AN.AnimeName aname, AN.GroupID gid, AN.EpisodeNO epno, AN.FileMask fmask, AN.AnimeMask amask]
        subFile netState (AnimeName aname) (GroupName gname) epno fmask amask = AN.getData netState "FILE" [AN.AnimeName aname, AN.GroupName gname, AN.EpisodeNO epno, AN.FileMask fmask, AN.AnimeMask amask]
