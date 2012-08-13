import AniReplyParse

main :: IO ()
main = do
    let foobar = parseAnidb $ testComp testString
    putStrLn $ show foobar
