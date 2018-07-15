import System.Environment

usage :: String
usage = "usage: hlox [script]"

parse :: [String] -> IO()
parse []      = runRepl
parse ["-h"]  = print usage
parse [file]  = runFile file
parse _       = print usage

run :: String -> IO()
run s = print s

runRepl :: IO()
runRepl = sequence_ takeInput
  where takeInput = (getLine >>= run) : takeInput

runFile :: String -> IO()
runFile f = readFile f >>= run

main :: IO ()
main = getArgs >>= parse
