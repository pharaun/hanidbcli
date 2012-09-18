import HClient.Options
import HClient.Hasher (fileDirectoryHash)
import HClient.Sync (fileDirectorySync)
import System.Console.CmdArgs

main :: IO ()
main = parseOptions =<< cmdArgsRun optionMode

parseOptions :: Options -> IO ()
parseOptions (Hash x) = print =<< fileDirectoryHash x
parseOptions (Sync x) = print =<< fileDirectorySync x
parseOptions x        = print x
