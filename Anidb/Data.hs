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

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import qualified Anidb.Network as AN
import qualified Anidb.Parse.Data as APD
import qualified Anidb.Parse.Reply as APR

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
creator :: AN.AniNetState -> Integer -> IO (APR.AniReplyParsed, Maybe APD.AniDataParsed)
creator netState cid = do
    reply <- AN.getData netState "CREATOR" [AN.CreatorID cid]
    return $ maybe (reply, Nothing) (\x -> (reply, Just $ APD.parseCreator x)) (APR.getBodyData reply)

-- TODO: get confirmation on the syntax of the episode list
-- Deals with the character data type/response
character :: AN.AniNetState -> Integer -> IO (APR.AniReplyParsed, Maybe APD.AniDataParsed)
character netState cid = do
    reply <- AN.getData netState "CHARACTER" [AN.CharacterID cid]
    return $ maybe (reply, Nothing) (\x -> (reply, Just $ APD.parseCharacter x)) (APR.getBodyData reply)

-- Calendar
calendar :: AN.AniNetState -> IO (APR.AniReplyParsed, Maybe APD.AniDataParsed)
calendar netState = do
    reply <- AN.getData netState "CALENDAR" []
    return $ maybe (reply, Nothing) (\x -> (reply, Just $ APD.parseCalendar x)) (APR.getBodyData reply)


-- TODO: Consider error reporting - IE Either (Either ParseError AniReply) String
animeDesc :: AN.AniNetState -> Integer -> IO String
animeDesc netState aid = concat <$> fetchParts 0
    where
        fetchParts :: Integer -> IO [String]
        fetchParts part = do
            reply <- AN.getData netState "ANIMEDESC" [AN.AnimeID aid, AN.DescPart part]

            flip (maybe (return [])) (APR.getBodyData reply) (\x -> do
                let desc = APD.parseAnimeDesc x
                    val  = extractVal desc

                if (moreParts desc)
                then (\s -> return $ [val] ++ s) =<< (fetchParts (part + 1))
                else return [val])

        moreParts :: APD.AniDataParsed -> Bool
        moreParts (Left _)  = False
        moreParts (Right x) = (APD.adCurrentPart x + 1) < APD.adMaxPart x

        extractVal :: APD.AniDataParsed -> String
        extractVal (Left _)  = ""
        extractVal (Right x) = APD.adDescription x


group :: AN.AniNetState -> GroupArg -> IO (APR.AniReplyParsed, Maybe APD.AniDataParsed)
group netState x = do
    reply <- AN.getData netState "GROUP" (getParm x)
    return $ maybe (reply, Nothing) (\x -> (reply, Just $ APD.parseGroup x)) (APR.getBodyData reply)
    where
        getParm :: GroupArg -> [AN.RequestOpt]
        getParm (GroupName x) = [AN.GroupName x]
        getParm (GroupID x)   = [AN.GroupID x]


-- TODO: get an actual sample of the syntax for the ep list
groupStatus :: AN.AniNetState -> Integer -> Maybe Integer -> IO (APR.AniReplyParsed, Maybe APD.AniDataParsed)
groupStatus netState aid state = do
    reply <- AN.getData netState "GROUPSTATUS" (getParm aid state)
    return $ maybe (reply, Nothing) (\x -> (reply, Just $ APD.parseGroupStatus x)) (APR.getBodyData reply)
    where
        getParm :: Integer -> Maybe Integer -> [AN.RequestOpt]
        getParm aid Nothing  = [AN.AnimeID aid]
        getParm aid (Just x) = [AN.AnimeID aid, AN.AnimeCompletionState x]


episode :: AN.AniNetState -> EpisodeArg -> IO (APR.AniReplyParsed, Maybe APD.AniDataParsed)
episode netState x = do
    reply <- AN.getData netState "EPISODE" (getParm x)
    return $ maybe (reply, Nothing) (\x -> (reply, Just $ APD.parseEpisode x)) (APR.getBodyData reply)
    where
        getParm :: EpisodeArg-> [AN.RequestOpt]
        getParm (EpisodeID eid)               = [AN.EpisodeID eid]
        getParm (EpisodeAnimeID aid epno)     = [AN.AnimeID aid, AN.EpisodeNO epno]
        getParm (EpisodeAnimeName aname epno) = [AN.AnimeName aname, AN.EpisodeNO epno]


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
