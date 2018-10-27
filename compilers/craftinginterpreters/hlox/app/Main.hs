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

run :: LoxState -> String -> IO LoxState
run state s = handleOutcome $ (normalize $ scanTokens s) >>=
                (normalize . parseStatement) >>=
                (normalize . (evaluateStatement state) . snd)
  where handleOutcome :: Either String (IO LoxState) -> IO LoxState
        handleOutcome (Right s) = s
        handleOutcome (Left o) = putStr ("There was an error! " ++  o ++ "\n") >> return state

runRepl :: IO ()
runRepl = (processInput zeroState) >> return ()
  where processInput :: LoxState -> IO [LoxState]
        processInput state = do
          s <- (getUserInput >>= run state)
          r <- (processInput s)
          return $ s:r
        getUserInput :: IO String
        getUserInput = putStr "> " >> hFlush stdout >> getLine

runFile :: String -> IO ()
runFile f = readFile f >>= run zeroState >> return ()

main :: IO ()
main = getArgs >>= parse