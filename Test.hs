import Anidb.Parse.Reply
import Anidb.Network
import Anidb.Data as D
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
--    forkChild ((threadDelay 1000000) >> (putStrLn "1") >> (auth a "ban" "test" True True) >>= (putStrLn . show))

    -- Management stuff
    forkChild ((threadDelay 2000000) >> (putStrLn "2") >> (ping a True) >>= (putStrLn . show))
    forkChild ((threadDelay 3000000) >> (putStrLn "3") >> (version a) >>= (putStrLn . show))
    forkChild ((threadDelay 4000000) >> (putStrLn "4") >> (uptime a) >>= (putStrLn . show))

    -- Data stuff
    forkChild ((threadDelay 5000000) >> (putStrLn "5") >> (creator a 718) >>= (putStrLn . show))
    forkChild ((threadDelay 5000000) >> (putStrLn "5") >> (creator a 1) >>= (putStrLn . show))

    forkChild ((threadDelay 6000000) >> (putStrLn "6") >> (character a 488) >>= (putStrLn . show))
    forkChild ((threadDelay 6000000) >> (putStrLn "6") >> (character a 1) >>= (putStrLn . show))

    forkChild ((threadDelay 7000000) >> (putStrLn "7") >> (calendar a) >>= (putStrLn . show))

    forkChild ((threadDelay 8000000) >> (putStrLn "8") >> (animeDesc a 3169) >>= (putStrLn . show))
    forkChild ((threadDelay 8000000) >> (putStrLn "8") >> (animeDesc a 1) >>= (putStrLn . show))
    forkChild ((threadDelay 8000000) >> (putStrLn "8") >> (animeDesc a 2) >>= (putStrLn . show))

    forkChild ((threadDelay 9000000) >> (putStrLn "9") >> (group a $ D.GroupID 7091) >>= (putStrLn . show))
    forkChild ((threadDelay 9000000) >> (putStrLn "9") >> (group a $ D.GroupID 3169) >>= (putStrLn . show))
    forkChild ((threadDelay 9000000) >> (putStrLn "9") >> (group a $ D.GroupName "Frostii") >>= (putStrLn . show))
    forkChild ((threadDelay 9000000) >> (putStrLn "9") >> (group a $ D.GroupName "NoName") >>= (putStrLn . show))

    forkChild ((threadDelay 10000000) >> (putStrLn "10") >> (groupStatus a 123 $ Just 1) >>= (putStrLn . show))
    forkChild ((threadDelay 10000000) >> (putStrLn "10") >> (groupStatus a 124 Nothing) >>= (putStrLn . show))
    forkChild ((threadDelay 10000000) >> (putStrLn "10") >> (groupStatus a 125 Nothing) >>= (putStrLn . show))

    forkChild ((threadDelay 11000000) >> (putStrLn "11") >> (episode a $ D.EpisodeID 2) >>= (putStrLn . show))
    forkChild ((threadDelay 11000000) >> (putStrLn "11") >> (episode a $ D.EpisodeAnimeID 1 2) >>= (putStrLn . show))
    forkChild ((threadDelay 11000000) >> (putStrLn "11") >> (episode a $ D.EpisodeAnimeName "Seikai no Monshou" 2) >>= (putStrLn . show))
    forkChild ((threadDelay 11000000) >> (putStrLn "11") >> (episode a $ D.EpisodeID 4) >>= (putStrLn . show))

    forkChild ((threadDelay 12000000) >> (putStrLn "12") >> (anime a (D.AnimeID 1) Nothing) >>= (putStrLn . show))
    forkChild ((threadDelay 12000000) >> (putStrLn "12") >> (anime a (D.AnimeName "Shikabane Hime") Nothing) >>= (putStrLn . show))
    forkChild ((threadDelay 12000000) >> (putStrLn "12") >> (anime a (D.AnimeID 1) (Just "b2f0e0fc000000")) >>= (putStrLn . show))
    forkChild ((threadDelay 12000000) >> (putStrLn "12") >> (anime a (D.AnimeName "Shikabane Hime") (Just "b2f0e0fc000000")) >>= (putStrLn . show))
    forkChild ((threadDelay 12000000) >> (putStrLn "12") >> (anime a (D.AnimeID 2) Nothing) >>= (putStrLn . show))

    forkChild ((threadDelay 13000000) >> (putStrLn "13") >> (file a (D.FileID 312498) "7FF8FEF8" "C000F0C0") >>= (putStrLn . show))
    forkChild ((threadDelay 13000000) >> (putStrLn "13") >> (file a (D.FileHash "70cd93fd3981cc80a8ea6a646ff805c9" 177747474) "7FF8FEF8" "C000F0C0") >>= (putStrLn . show))
    forkChild ((threadDelay 13000000) >> (putStrLn "13") >> (file a (D.FileAnimeGroup (D.AnimeID 4688) (D.GroupID 4243) 2) "7FF8FEF8" "C000F0C0") >>= (putStrLn . show))
    forkChild ((threadDelay 13000000) >> (putStrLn "13") >> (file a (D.FileAnimeGroup (D.AnimeID 4688) (D.GroupName "Frostii") 2) "7FF8FEF8" "C000F0C0") >>= (putStrLn . show))
    forkChild ((threadDelay 13000000) >> (putStrLn "13") >> (file a (D.FileAnimeGroup (D.AnimeName "Sora he no Tsubasa") (D.GroupID 4243) 2) "7FF8FEF8" "C000F0C0") >>= (putStrLn . show))
    forkChild ((threadDelay 13000000) >> (putStrLn "13") >> (file a (D.FileAnimeGroup (D.AnimeName "Sora he no Tsubasa") (D.GroupName "Frostii") 2) "7FF8FEF8" "C000F0C0") >>= (putStrLn . show))
    forkChild ((threadDelay 13000000) >> (putStrLn "13") >> (file a (D.FileID 312499) "7FF8FEF8" "C000F0C0") >>= (putStrLn . show))

    -- Deauth
    forkChild ((threadDelay 14000000) >> (putStrLn "20") >> (logout a) >>= (putStrLn . show))

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
