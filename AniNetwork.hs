module AniNetwork
    ( connect
    , disconnect
    ) where

import Network.BSD (HostName)
import Network.HTTP.Base (urlEncodeVars)
import qualified AniRawNetwork as AR
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- configuration
data AniNetConf = AniNetConf {
    ancHostName :: HostName
    , ancPort :: Integer
    , ancProtoVer :: Integer
    , ancClient :: String
    , ancClientVer :: Integer
    }

-- Stateful stuff here
data AniNetState = AniNetState {
    ansSocket :: AR.AniRawSocket
    }

-- Setup connections
connect :: AniNetConf -> IO AniNetState
connect netcfg = do
    aniRawSocket <- AR.connect (ancHostName netcfg) (show $ ancPort netcfg)
    return $ AniNetState aniRawSocket

disconnect :: AniNetState -> IO ()
disconnect netState = AR.disconnect (ansSocket netState)

-- Misc command support
-- TODO: should take care of encoding
-- TODO: Take care of possible failure states
ping :: AniNetState -> Bool -> IO B.ByteString
ping netState nat =
    (AR.sendReq (ansSocket netState) msg)
    >> AR.recvReply (ansSocket netState)
    where
        msg = if nat then genReq "PING" $ Just [("nat", "1")]
            else genReq "PING" Nothing

version :: AniNetState -> IO B.ByteString
version netState =
    (AR.sendReq (ansSocket netState) $ genReq "VERSION" Nothing)
    >> AR.recvReply (ansSocket netState)

uptime :: AniNetState -> IO B.ByteString
uptime netState =
    (AR.sendReq (ansSocket netState) $ genReq "UPTIME" Nothing)
    >> AR.recvReply (ansSocket netState)


-- Supporting code
type RequestType = String

genReq :: RequestType -> Maybe [(String, String)] -> B.ByteString
genReq req Nothing    = C.pack req
genReq req (Just opt) = C.pack (req ++ " " ++ (urlEncodeVars opt))


-- Test stuff here
defaultConf :: AniNetConf
defaultConf = AniNetConf "localhost" 9000 3 "anidbcli" 1
