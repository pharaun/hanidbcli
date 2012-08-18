--
-- This module concerns itself with the raw network management stuff
-- such as rate that packets are being sent and things of this nature
-- this layer should have minimal application intelligence, instead
-- it should offload the session/login/etc stuff to a higher layer
--
-- Basically it regulates the send/recieve rate, maybe manage the queue
-- for sending/recieving data, and so forth...
--
-- May want to tweak the api slightly to externalize the actual act of sending,
-- and make the sendReq basically append a request to a list, then have a processSend
-- or some such actually do the sending so you can slot it into your event loop
-- or in my case just slap a thread over it
--
module AniRawNetwork
    ( connect
    , disconnect
    , sendReq
    , recvReply
    ) where

import Data.List
import Network.BSD
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as B


data AniRawSocket = AniRawSocket {
    arSocket :: NS.Socket
    , arAddress :: NS.SockAddr
    }


connect :: HostName -> String -> IO AniRawSocket
connect hostname port = do
        -- Look up hostname & port, this rases an exception if it
        -- fails. Just grab the first element in that list
       addrInfo <- head `fmap` NS.getAddrInfo Nothing (Just hostname) (Just port)

       -- Establish a socket for communication
       sock <- NS.socket (NS.addrFamily addrInfo) NS.Datagram defaultProtocol

       -- Save off the socket, program name, and server address in a handle
       return $ AniRawSocket sock (NS.addrAddress addrInfo)

disconnect :: AniRawSocket -> IO ()
disconnect handle = NS.sClose (arSocket handle)

sendReq :: AniRawSocket -> B.ByteString -> IO ()
sendReq handle msg = NSB.sendAllTo (arSocket handle) msg (arAddress handle)

recvReply = undefined
