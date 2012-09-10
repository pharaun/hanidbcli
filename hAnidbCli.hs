import HClient.Options
import System.Console.CmdArgs

main :: IO ()
main = print =<< cmdArgsRun optionMode
