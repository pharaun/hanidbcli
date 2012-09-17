import HClient.Options
import HClient.Hasher (fileDirectoryHash)
import System.Console.CmdArgs

main :: IO ()
main = parseOptions =<< cmdArgsRun optionMode

parseOptions :: Options -> IO ()
parseOptions (Hash x) = print =<< fileDirectoryHash x
parseOptions x        = print x
