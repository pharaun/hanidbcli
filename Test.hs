import AniReplyParse
import AniNetwork
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import Control.Concurrent.MVar
import Control.Exception.Base (finally)
import Control.Monad (forever)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
    -- network stuff
    a <- connect defaultConf

    -- Spin up the reciever
    recv <- forkIO (forever $ processRecieved a)
    
    -- fork off each message for testing reciever stuff
    forkChild ((threadDelay 1000000) >> (putStrLn "1") >> (auth a "test" "test" True True) >>= (putStrLn . show))
    forkChild ((threadDelay 2000000) >> (putStrLn "2") >> (ping a True) >>= (putStrLn . show))
    forkChild ((threadDelay 3000000) >> (putStrLn "3") >> (version a) >>= (putStrLn . show))
    forkChild ((threadDelay 4000000) >> (putStrLn "4") >> (uptime a) >>= (putStrLn . show))
    forkChild ((threadDelay 5000000) >> (putStrLn "5") >> (logout a) >>= (putStrLn . show))

    -- Join all of the thread, then kill the reciever and disconnect
    waitForChildren
    killThread recv
    
    disconnect a


-- TODO: this code below is meh but it'll work for now for thread blocking/etc
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())
