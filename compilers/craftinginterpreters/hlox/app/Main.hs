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

normalize :: (Show s) => Either (ProgramError s) a -> Either String a
normalize (Left o) = Left $ show o
normalize (Right o) = Right o

run :: String -> IO()
run s = handleOutcome ((normalize $ scanTokens s) >>= (normalize . parseExpression))
  where handleOutcome :: Either String ParsingStep -> IO()
        handleOutcome (Right o) = putStrLn (show o)
        handleOutcome (Left o) = die (show o)

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
