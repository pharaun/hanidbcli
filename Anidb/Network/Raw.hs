module Anidb.Network.Raw
    ( connect
    , disconnect
    , sendReq
    , recvReply

    -- Socket/data
    , AniRawSocket
    ) where

import Data.Functor ((<$>))
import Data.List (head)
import Network.BSD (HostName)
import qualified Data.ByteString as B
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

-- Types
data AniRawSocket = AniRawSocket {
    arSocket :: NS.Socket
    , arAddress :: NS.SockAddr
    }


connect :: HostName -> NS.PortNumber -> NS.PortNumber -> IO AniRawSocket
connect hostname dstPort srcPort = do
    -- Look up hostname & port, this rases an exception if it
    -- fails. Just grab the first element in that list
    addrInfo <- head <$> NS.getAddrInfo Nothing (Just hostname) (Just $ show dstPort)

    -- Establish a socket for communication
    sock <- NS.socket (NS.addrFamily addrInfo) NS.Datagram NS.defaultProtocol

    -- Bind
--    localHost <- NS.inet_addr "0.0.0.0"
--    NS.bindSocket sock $ NS.SockAddrInet srcPort localHost

    -- Connect
    NS.connect sock (NS.addrAddress addrInfo)

    -- Save off the socket, program name, and server address in a handle
    return $ AniRawSocket sock (NS.addrAddress addrInfo)

disconnect :: AniRawSocket -> IO ()
disconnect handle = NS.sClose (arSocket handle)

-- TODO: add support for adjusting/regulating how fast requests
-- are being sent out
sendReq :: AniRawSocket -> B.ByteString -> IO ()
sendReq handle = NSB.sendAll (arSocket handle)

recvReply :: AniRawSocket -> IO B.ByteString
recvReply handle = NSB.recv (arSocket handle) 1500
