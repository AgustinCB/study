import Hlox
import System.Environment
import System.Exit
import System.IO

usage :: String
usage = "usage: hlox [script]"

parse :: [String] -> IO()
parse []      = runRepl
parse ["-h"]  = putStrLn usage
parse [file]  = runFile file
parse _       = putStrLn usage

getOutcome :: String -> ProgramOutcome
getOutcome m = Success m

run :: String -> IO()
run s = handleOutcome (getOutcome s)
  where handleOutcome :: ProgramOutcome -> IO()
        handleOutcome o@(Success _) = putStrLn (show o)
        handleOutcome o@(Error _ _) = die (show o)

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
