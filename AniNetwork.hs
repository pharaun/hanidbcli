module AniNetwork
    ( connect
    , disconnect

    , auth
    , ping
    , version
    , uptime

    , defaultConf
    ) where

import Control.Concurrent.MVar
import Data.Maybe (fromJust, isJust)
import Network.BSD (HostName)
import Network.HTTP.Base (urlEncodeVars)
import qualified AniRawNetwork as AR
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

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
    , ansSession :: (MVar Session)
    }

-- Session type
type Session = String

-- Setup connections
connect :: AniNetConf -> IO AniNetState
connect netcfg = do
    aniRawSocket <- AR.connect (anidbHostName netcfg) (show $ anidbPort netcfg)
    newSession <- newEmptyMVar
    return $ AniNetState netcfg aniRawSocket newSession

disconnect :: AniNetState -> IO ()
disconnect netState = AR.disconnect (ansSocket netState)

-- Auth/Session command support
auth :: AniNetState -> String -> String -> Bool -> Bool -> IO B.ByteString
auth netState user pass nat imgserver =
    (AR.sendReq (ansSocket netState) (genReq "AUTH" $ Just optList))
    >> AR.recvReply (ansSocket netState)
    where
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

-- Misc command support
-- TODO: should take care of encoding
-- TODO: Take care of possible failure states
ping :: AniNetState -> Bool -> IO B.ByteString
ping netState nat =
    (AR.sendReq (ansSocket netState) $ genReq "PING" $ Just [NAT nat])
    >> AR.recvReply (ansSocket netState)

version :: AniNetState -> IO B.ByteString
version netState =
    (AR.sendReq (ansSocket netState) $ genReq "VERSION" Nothing)
    >> AR.recvReply (ansSocket netState)

uptime :: AniNetState -> IO B.ByteString
uptime netState =
    (readMVar (ansSession netState)) >>= (\s ->
    (AR.sendReq (ansSocket netState) $ genReq "UPTIME" $ Just [SessionOpt s]))
    >> AR.recvReply (ansSocket netState)


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
        optStr _                    = Nothing



-- TODO: Wrap/add support for random generator
genTag :: AniNetState -> String
genTag netState = "TESTTAG"


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
                | SessionOpt Session
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

