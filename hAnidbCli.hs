import HClient.Options
import System.Console.CmdArgs

testFile :: FilePath
testFile = "test.mkv"

main :: IO ()
main = print =<< cmdArgsRun optionMode
