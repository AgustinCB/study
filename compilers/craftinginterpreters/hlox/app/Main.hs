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

run :: String -> IO()
run s = handleOutcome $ scanTokens s >>= parseExpression >>= (evaluate . fst)
  where handleOutcome :: (Show r, Show o) => Either (ProgramError r) o -> IO()
        handleOutcome (Right o) = putStrLn (show o)
        handleOutcome (Left o) = die $ "There was an error! " ++  (show o)

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
