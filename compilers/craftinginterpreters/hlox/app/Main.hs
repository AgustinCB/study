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
                (normalize . parseExpression) >>=
                (normalize . (evaluateExpression state) . snd)
  where handleOutcome :: Either String (LoxState, LoxValue) -> IO LoxState
        handleOutcome (Right (s, v)) = putStrLn (show v) >> return s
        handleOutcome (Left o) = die $ "There was an error! " ++  o

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