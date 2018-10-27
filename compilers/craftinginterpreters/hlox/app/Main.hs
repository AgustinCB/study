import Hlox
import System.Environment
import System.Exit
import System.IO

import Control.Monad (join)

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
run state s = handleOutcome $ case tokenResult of Right (_, s) -> fmap normalize $ evaluateStatement state s
                                                  Left e -> return $ Left e
  where handleOutcome :: IO (Either String LoxState) -> IO LoxState
        handleOutcome = join . (fmap handleOutcome')
        handleOutcome' :: Either String LoxState -> IO LoxState
        handleOutcome' (Right s) = return s
        handleOutcome' (Left o) = putStr ("There was an error! " ++  o ++ "\n") >> return state
        tokenResult :: Either String ([Token], Statement)
        tokenResult = (normalize $ scanTokens s) >>= (normalize . parseStatement)

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