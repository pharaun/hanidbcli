import HClient.Options
import HClient.Hasher (fileDirectoryHash)
import HClient.Sync (fileDirectorySync)
import HClient.SyncHash (fileDirectorySyncHash)
import System.Console.CmdArgs

main :: IO ()
main = parseOptions =<< cmdArgsRun optionMode

parseOptions :: Options -> IO ()
parseOptions (Hash x)     = print =<< fileDirectoryHash x
parseOptions (Sync x)     = print =<< fileDirectorySync x
parseOptions (SyncHash x) = print =<< fileDirectorySyncHash x
parseOptions x            = print x
