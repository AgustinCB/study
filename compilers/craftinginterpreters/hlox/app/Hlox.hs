import System.Environment

usage :: String
usage = "usage: hlox [script]"

parse :: [String] -> IO()
parse []      = runRepl
parse ["-h"]  = print usage
parse [file]  = runFile
parse _       = print usage

runRepl :: IO()
runRepl = print "THIS IS A REPL"

runFile :: IO()
runFile = print "THIS IS A FILE"

main :: IO ()
main = getArgs >>= parse
