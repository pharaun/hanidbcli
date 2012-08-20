import AniReplyParse
import AniNetwork

main :: IO ()
main = do
    let foobar = parseAnidb $ testComp testString
    putStrLn $ show foobar

    -- network stuff
    a <- connect defaultConf
    b <- ping a True
    putStrLn $ show b
    c <- auth a "test" "test" True True
    putStrLn $ show c
    d <- uptime a
    putStrLn $ show d
    e <- logout a
    putStrLn $ show e
    f <- version a
    putStrLn $ show f
    disconnect a
