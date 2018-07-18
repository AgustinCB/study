import Hlox
import System.Environment
import System.IO

report :: SourceCodeLocation -> String -> IO()
report loc msg = hPutStrLn stderr $ (show loc) ++ msg

error :: Int -> String -> IO()
error line msg = report (mkSourceCodeLocation Nothing line) msg

usage :: String
usage = "usage: hlox [script]"

parse :: [String] -> IO()
parse []      = runRepl
parse ["-h"]  = putStrLn usage
parse [file]  = runFile file
parse _       = putStrLn usage

run :: String -> IO()
run s = putStrLn s

runRepl :: IO()
runRepl = sequence_ processInput
  where processInput :: [IO()]
        processInput = (getUserInput >>= run) : processInput
        getUserInput :: IO String
        getUserInput = putStr "> " >> hFlush stdout >> getLine

runFile :: String -> IO()
runFile f = readFile f >>= run

main :: IO ()
main = getArgs >>= parse
