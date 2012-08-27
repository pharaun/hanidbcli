module Anidb.Data
    ( anime
    , animeDesc
    , calendar
    , character
    , creator
    , episode
    , file
    , group
    , groupStatus
    ) where

import qualified Anidb.Network as AN
import qualified Anidb.Parse.Reply as APR
import qualified Anidb.Parse.Data as APD

anime = undefined
episode = undefined
file = undefined
group = undefined
groupStatus = undefined

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
