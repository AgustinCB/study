import System.Environment
import System.IO

usage :: String
usage = "usage: hlox [script]"

parse :: [String] -> IO()
parse []      = runRepl
parse ["-h"]  = putStrLn usage
parse [file]  = runFile file
parse _       = putStrLn usage

run :: String -> IO()
run s = putStrLn s

getUserInput :: IO String
getUserInput = putStr "> " >> hFlush stdout >> getLine

runRepl :: IO()
runRepl = sequence_ takeInput
  where takeInput = (getUserInput >>= run) : takeInput

runFile :: String -> IO()
runFile f = readFile f >>= run

main :: IO ()
main = getArgs >>= parse
