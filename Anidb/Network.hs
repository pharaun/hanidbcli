module Anidb.Network
    ( connect
    , disconnect
    , processRecieved

    -- Control
    , auth
    , logout
    , ping
    , version
    , uptime

    -- Data
    , getData
    , RequestType
    , RequestOpt(..)

    -- Type
    , AniNetState

    , defaultConf
    , anidbConf
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Data.Functor ((<$>))
import Data.Maybe (fromJust, isJust)
import Network.BSD (HostName)
import Network.HTTP.Base (urlEncodeVars)
import Network.Socket (PortNumber)
import qualified Anidb.Network.Raw as AR
import qualified Anidb.Parse.Reply as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

-- Types
data AniNetConf = AniNetConf {
    -- Server
    anidbHostName :: HostName
    , anidbPort :: PortNumber
    , anidbProtoVer :: Integer
    -- Client specific
    , clientPort :: PortNumber
    , clientName :: String
    , clientVer :: Integer
    -- Optional support
    , clientEncode :: Maybe String
    , clientMTU :: Maybe Integer
    , clientCompress :: Bool
    }

-- Stateful network
data AniNetState = AniNetState {
    ansConfig :: AniNetConf
    , ansSocket :: AR.AniRawSocket
    , ansSession :: (MVar Session)
    , ansKey :: (MVar Int)
    -- Tag, data
    , ansResult :: (MVar (M.Map Tag (MVar AP.AniReplyParsed)))
    }

-- TODO: newtype these
type Tag = String
type Session = String
type RequestType = String

-- Internal request option mapping
data InternalRequestOpt =
    -- Auth Request Options
    UserName String | Password String
    | AniDBProtoVer Integer | ClientVer Integer
    | ClientName String
    | NAT Bool | ImgServer Bool | Compression Bool
    | Encode (Maybe String) | MTU (Maybe Integer)
    -- Session Management
    | SessionOpt Session
    -- Tag
    | TagOpt Tag
    -- Encryption - TODO: link to encryption Type
    | EncryptionType Integer

-- External request option mapping for all of the external data requests
data RequestOpt =
    -- Notification Commands - TODO: Never has tags, special stuff
    -- Buddy Commands
    -- Anime Data - TODO: Link to mask data
    AnimeID Integer | AnimeName String | AnimeMask String -- TODO: probably need a long/short ver
    -- Anime Description
    | DescPart Integer
    -- Character Data
    | CharacterID Integer
    -- Creator Data
    | CreatorID Integer
    -- Episode Data
    | EpisodeID Integer | EpisodeNO Integer
    -- File Data - TODO: Link to mask data
    | FileID Integer | FileMask String | FileSize Integer | FileHash String
    -- Group Data
    | GroupID Integer | GroupName String
    -- Group Status - TODO: link to state integer
    | AnimeCompletionState Integer
    -- MyList Commands

-- Anidb Request
data AniRequest = AniRequest RequestType [InternalRequestOpt] [RequestOpt]

-- Session flag
data SessionFlag = NoSession | ReadSession | TakeSession


-- Setup connections
connect :: AniNetConf -> IO AniNetState
connect netcfg = do
    aniRawSocket <- AR.connect (anidbHostName netcfg) (anidbPort netcfg) (clientPort netcfg)
    newSession <- newEmptyMVar
    newKey <- newMVar 0
    newResult <- newMVar M.empty
    return $ AniNetState netcfg aniRawSocket newSession newKey newResult

-- TODO: do we want to deauth if we are auth here, and reset all of the MVars?
disconnect :: AniNetState -> IO ()
disconnect netState = AR.disconnect (ansSocket netState)


-- Deal with processing the recieved packets
processRecieved :: AniNetState -> IO ()
processRecieved netState = do
    reply <- AR.recvReply (ansSocket netState)
    forkIO (notifySenderThread netState reply)
    return ()

notifySenderThread :: AniNetState -> B.ByteString -> IO ()
notifySenderThread netState reply = do
    let parsedReply = AP.parseAniReply $ L.fromChunks [reply]
        tag = AP.getTag parsedReply

    processTag tag parsedReply
    where
        processTag :: Maybe Tag -> AP.AniReplyParsed -> IO ()
        processTag Nothing  y = putStrLn ("No tag on reply: " ++ show y)
        processTag (Just x) y = (modifyMVar_ (ansResult netState) (\m -> do
                updateTagMVar (M.lookup x m) x y
                return (m)
            ))

        -- TODO: Add more intelligence here, if theres only one request sent and
        -- an error is returned without tag its probably that request, etc
        updateTagMVar :: Maybe (MVar a) -> Tag -> a -> IO ()
        updateTagMVar Nothing  y _ = putStrLn ("Tag does not exist: " ++ y)
        updateTagMVar (Just x) _ z = putMVar x z


-- Deal with sending a packet - TODO: Deal with timeouts/retry
sendRequest :: AniNetState -> SessionFlag -> AniRequest -> IO AP.AniReplyParsed
sendRequest netState ses req = do
    s <- getSession ses
    t <- genTag netState
    AR.sendReq (ansSocket netState) $ genReq $ updateAniRequest req (s ++ [TagOpt t])
    storeTag netState t
    where
        getSession :: SessionFlag -> IO [InternalRequestOpt]
        getSession ReadSession = (\s -> return [SessionOpt s]) =<< readMVar (ansSession netState)
        getSession TakeSession = (\s -> return [SessionOpt s]) =<< takeMVar (ansSession netState)
        getSession _ = return []

        updateAniRequest :: AniRequest -> [InternalRequestOpt] -> AniRequest
        updateAniRequest (AniRequest t i o) ni = AniRequest t (i ++ ni) o

-- Store the tag of the request and then block for the reply, then cleanup the tag
storeTag :: AniNetState -> Tag -> IO AP.AniReplyParsed
storeTag netState tag = do
    newTagMVar <- newEmptyMVar
    modifyMVar_ (ansResult netState) (\m -> return (M.insert tag newTagMVar m))
    reply <- takeMVar newTagMVar
    modifyMVar_ (ansResult netState) (\m -> return (M.delete tag m))
    return reply


-- Auth to the service and set the session token
auth :: AniNetState -> String -> String -> Bool -> Bool -> IO AP.AniReplyParsed
auth netState user pass nat imgserver = do
    parsedReply <- sendRequest netState NoSession $ AniRequest "AUTH" optList []
    updateSession $ AP.getSession parsedReply
    return parsedReply
    where
        -- TODO: This is not txn safe at all
        updateSession :: (Maybe String) -> IO ()
        updateSession (Just x) = (tryTakeMVar (ansSession netState)) >> (putMVar (ansSession netState) x)
        updateSession Nothing  = return ()

        cfg = ansConfig netState
        optList = [
            UserName user, Password pass,
            (AniDBProtoVer $ anidbProtoVer cfg),
            (ClientVer $ clientVer cfg),
            (ClientName $ clientName cfg),
            (Compression $ clientCompress cfg),
            (Encode $ clientEncode cfg),
            (MTU $ clientMTU cfg),
            NAT nat, ImgServer imgserver]
    

-- Logout - TODO: probably shouldn't be blocking, should take and fail fast or deauth
logout :: AniNetState -> IO AP.AniReplyParsed
logout netState = sendRequest netState TakeSession $ AniRequest "LOGOUT" [] []


-- Misc command support - TODO: Take care of possible failure states
ping :: AniNetState -> Bool -> IO AP.AniReplyParsed
ping netState nat = sendRequest netState NoSession $ AniRequest "PING" [NAT nat] []

version :: AniNetState -> IO AP.AniReplyParsed
version netState = sendRequest netState NoSession $ AniRequest "VERSION" [] []

uptime :: AniNetState -> IO AP.AniReplyParsed
uptime netState = sendRequest netState ReadSession $ AniRequest "UPTIME" [] []


-- Broad command support for the data subtype
getData :: AniNetState -> RequestType -> [RequestOpt] -> IO AP.AniReplyParsed
getData netState req reqOpt = sendRequest netState ReadSession $ AniRequest req [] reqOpt


-- Supporting code - TODO: should take care of encoding
genReq :: AniRequest -> B.ByteString
genReq (AniRequest req opt reqOpt) = C.pack (req ++ " " ++ (urlEncodeVars $ optToStr opt reqOpt))
    where
        optToStr :: [InternalRequestOpt] -> [RequestOpt] -> [(String, String)]
        optToStr opt reqOpt = map fromJust $ filter isJust $ (map iOptStr opt ++ map optStr reqOpt)

        -- Internal Request Options
        iOptStr :: InternalRequestOpt -> Maybe (String, String)
        iOptStr (AniDBProtoVer x)    = Just ("protover", show x)
        iOptStr (ClientName x)       = Just ("client", x)
        iOptStr (ClientVer x)        = Just ("clientver", show x)
        iOptStr (Compression True)   = Just ("comp", "1")
        iOptStr (Encode (Just x))    = Just ("enc", x)
        iOptStr (ImgServer True)     = Just ("imgserver", "1")
        iOptStr (MTU (Just x))       = Just ("mtu", show x)
        iOptStr (NAT True)           = Just ("nat", "1")
        iOptStr (Password x)         = Just ("pass", x)
        iOptStr (SessionOpt x)       = Just ("s", x)
        iOptStr (TagOpt x)           = Just ("tag", x)
        iOptStr (UserName x)         = Just ("user", x)
        iOptStr _                    = Nothing

        -- Request Options (Data/etc)
        optStr :: RequestOpt -> Maybe (String, String)
        optStr (AnimeCompletionState x) = Just ("state", show x)
        optStr (AnimeID x)              = Just ("aid", show x)
        optStr (AnimeMask x)            = Just ("amask", x)
        optStr (AnimeName x)            = Just ("aname", x)
        optStr (CharacterID x)          = Just ("charid", show x)
        optStr (CreatorID x)            = Just ("creatorid", show x)
        optStr (DescPart x)             = Just ("part", show x)
        optStr (EpisodeID x)            = Just ("eid", show x)
        optStr (EpisodeNO x)            = Just ("epno", show x)
        optStr (FileHash x)             = Just ("ed2k", x)
        optStr (FileID x)               = Just ("fid", show x)
        optStr (FileMask x)             = Just ("fmask", x)
        optStr (FileSize x)             = Just ("size", show x)
        optStr (GroupID x)              = Just ("gid", show x)
        optStr (GroupName x)            = Just ("gname", x)


-- Deals with generating the new tag
genTag :: AniNetState -> IO Tag
genTag netState =
    modifyMVar (ansKey netState) (\oldKey -> do
        let newKey = if oldKey+1 >= cycleTime then 0 else oldKey+1
        return (newKey, genTag' tagLength newKey))
        where
            tagLength = 4
            cycleTime = length charOrder^tagLength

genTag' :: Int -> Int -> Tag
genTag' n x = reverse ((charOrder !!) <$> fapp n x)

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

anidbConf :: AniNetConf
anidbConf = AniNetConf "api.anidb.net" 9000 3 10000 "anidbcli" 1 (Just "UTF8") Nothing False
