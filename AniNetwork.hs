module AniNetwork
    ( connect
    , disconnect

    , auth
    , logout
    , ping
    , version
    , uptime

    , defaultConf
    ) where

import Control.Concurrent.MVar
import Data.Functor ((<$>))
import Data.Maybe (fromJust, isJust)
import Network.BSD (HostName)
import Network.HTTP.Base (urlEncodeVars)
import qualified AniRawNetwork as AR
import qualified AniReplyParse as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

-- configuration
data AniNetConf = AniNetConf {
    -- Server
    anidbHostName :: HostName
    , anidbPort :: Integer
    , anidbProtoVer :: Integer
    -- Client specific
    , clientPort :: Integer
    , clientName :: String
    , clientVer :: Integer
    -- Optional support
    , clientEncode :: Maybe String
    , clientMTU :: Maybe Integer
    , clientCompress :: Bool
    }

-- Stateful stuff here
data AniNetState = AniNetState {
    ansConfig :: AniNetConf
    , ansSocket :: AR.AniRawSocket
    -- TODO: add some mvars as needed probably
    -- such as rnd gen stuff, session/login/out stuff
    , ansSession :: (MVar String)
    , ansKey :: (MVar Int)
    }

-- Setup connections
connect :: AniNetConf -> IO AniNetState
connect netcfg = do
    aniRawSocket <- AR.connect (anidbHostName netcfg) (show $ anidbPort netcfg)
    newSession <- newEmptyMVar
    newKey <- newMVar 0
    return $ AniNetState netcfg aniRawSocket newSession newKey

disconnect :: AniNetState -> IO ()
disconnect netState = AR.disconnect (ansSocket netState)

-- Deal with recieving replies and parsing
parseReply :: B.ByteString -> Either AP.ParseError AP.AniReply
parseReply a = AP.parseAnidb $ L.fromChunks [a]

-- Auth/Session command support
auth :: AniNetState -> String -> String -> Bool -> Bool -> IO (Either AP.ParseError AP.AniReply)
auth netState user pass nat imgserver = do
    tag <- genTag netState
    AR.sendReq (ansSocket netState) (genReq "AUTH" (Just $ optList tag))
    reply <- AR.recvReply (ansSocket netState)
    let parsedReply = parseReply reply

    -- Write a new session
    updateSession $ AP.getSession parsedReply

    return parsedReply

    where
        -- TODO: This is not txn safe at all
        updateSession :: (Maybe String) -> IO ()
        updateSession (Just x) = (tryTakeMVar (ansSession netState)) >> (putMVar (ansSession netState) x)
        updateSession Nothing  = return ()

        cfg = ansConfig netState
        optList tag = [
            UserName user, Password pass,
            (AniDBProtoVer $ anidbProtoVer cfg),
            (ClientVer $ clientVer cfg),
            (ClientName $ clientName cfg),
            (Compression $ clientCompress cfg),
            (Encode $ clientEncode cfg),
            (MTU $ clientMTU cfg),
            NAT nat, ImgServer imgserver,
            Tag tag]

-- Logout
logout :: AniNetState -> IO (Either AP.ParseError AP.AniReply)
logout netState = do
    s <- takeMVar (ansSession netState)
    tag <- genTag netState
    AR.sendReq (ansSocket netState) $ genReq "LOGOUT" $ Just [SessionOpt s, Tag tag]
    reply <- AR.recvReply (ansSocket netState)
    return $ parseReply reply

-- Misc command support
-- TODO: should take care of encoding
-- TODO: Take care of possible failure states
ping :: AniNetState -> Bool -> IO (Either AP.ParseError AP.AniReply)
ping netState nat = do
    tag <- genTag netState
    AR.sendReq (ansSocket netState) $ genReq "PING" $ Just [NAT nat, Tag tag]
    reply <- AR.recvReply (ansSocket netState)
    return $ parseReply reply

version :: AniNetState -> IO (Either AP.ParseError AP.AniReply)
version netState = do
    tag <- genTag netState
    AR.sendReq (ansSocket netState) $ genReq "VERSION" $ Just [Tag tag]
    reply <- AR.recvReply (ansSocket netState)
    return $ parseReply reply

uptime :: AniNetState -> IO (Either AP.ParseError AP.AniReply)
uptime netState = do
    s <- readMVar (ansSession netState)
    tag <- genTag netState
    AR.sendReq (ansSocket netState) $ genReq "UPTIME" $ Just [SessionOpt s, Tag tag]
    reply <- AR.recvReply (ansSocket netState)
    return $ parseReply reply



-- Supporting code
type RequestType = String

genReq :: RequestType -> Maybe [RequestOpt] -> B.ByteString
genReq req Nothing    = C.pack req
genReq req (Just opt) = C.pack (req ++ " " ++ (urlEncodeVars $ optToStr opt))
    where
        optToStr :: [RequestOpt] -> [(String, String)]
        optToStr reqOpt = map fromJust $ filter isJust $ map optStr reqOpt

        optStr :: RequestOpt -> Maybe (String, String)
        optStr (UserName x)         = Just ("user", x)
        optStr (Password x)         = Just ("pass", x)
        optStr (AniDBProtoVer x)    = Just ("protover", show x)
        optStr (ClientVer x)        = Just ("clientver", show x)
        optStr (ClientName x)       = Just ("client", x)
        optStr (NAT True)           = Just ("nat", "1")
        optStr (ImgServer True)     = Just ("imgserver", "1")
        optStr (Compression True)   = Just ("comp", "1")
        optStr (Encode (Just x))    = Just ("enc", x)
        optStr (MTU (Just x))       = Just ("mtu", show x)
        optStr (SessionOpt x)       = Just ("s", x)
        optStr (Tag x)              = Just ("tag", x)
        optStr _                    = Nothing

genTag :: AniNetState -> IO String
genTag netState =
    modifyMVar (ansKey netState) (\oldKey -> do
        let newKey = if oldKey+1 >= cycleTime then 0 else oldKey+1
        return (newKey, genTag' tagLength newKey))
        where
            tagLength = 4
            cycleTime = length charOrder^tagLength

genTag' :: Int -> Int -> String
genTag' n x = (charOrder !!) <$> fapp n x

fapp :: Int -> Int -> [Int]
fapp n x = mod' <$> take n (iterate div' x)
    where
        len = length charOrder
        mod' = flip mod len
        div' = flip div len

charOrder :: String
charOrder = ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'9']



-- Test stuff here
defaultConf :: AniNetConf
defaultConf = AniNetConf "localhost" 9000 3 10000 "anidbcli" 1 (Just "UTF8") Nothing False


-- Request option data
data RequestOpt =
                -- Auth Request Options
                UserName String | Password String
                | AniDBProtoVer Integer | ClientVer Integer
                | ClientName String
                | NAT Bool | ImgServer Bool
                | Compression Bool | Encode (Maybe String)
                | MTU (Maybe Integer)
                -- Session Management
                | SessionOpt String
                -- Tag
                | Tag String
                -- Encryption
                | EncryptionType Integer -- TODO: link to encryption Type
                -- Notification Commands
                -- Buddy Commands
                -- Anime Data
                | AnimeID Integer | AnimeName String
                | AnimeMask String -- TODO: Link to mask data
                -- Anime Description
                | DescPart Integer
                -- Character Data
                | CharacterID Integer
                -- Creator Data
                | CreatorID Integer
                -- Episode Data
                | EpisodeID Integer | EpisodeNO Integer
                -- File Data
                | FileID Integer | FileMask String -- TODO: link to mask data
                | FileSize Integer | FileHash String
                -- Group Data
                | GroupID Integer | GroupName String
                -- Group Status
                | AnimeCompletionState Integer -- TODO: link to state integer
                -- MyList Commands

