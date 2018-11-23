import Hlox
import System.Environment
import System.Exit
import System.IO

import Control.Applicative (liftA2)
import Control.Monad (join)

usage :: String
usage = "usage: hlox [script]"

putStrLnError :: String -> IO ()
putStrLnError s = hPutStr stderr (s ++ "\n")

parse :: [String] -> IO()
parse []      = runRepl
parse ["-h"]  = putStrLn usage
parse [file]  = runFile file
parse _       = putStrLn usage

normalize :: (Show s) => Either (ProgramError s) a -> Either String a
normalize (Left o) = Left $ show o
normalize (Right o) = Right o

stringToStatements :: String -> Either String [Statement]
stringToStatements content = (normalize $ scanTokens content) >>= tokensToResult
  where tokensToResult :: [Token] -> Either String [Statement]
        tokensToResult tokens = (normalize $ parseStatement tokens False) >>= uncurry tokensAndStatementToResult
        tokensAndStatementToResult :: [Token] -> Statement -> Either String [Statement]
        tokensAndStatementToResult [] statement = Right [statement]
        tokensAndStatementToResult tokens statement = liftA2 (:) (Right statement) (tokensToResult tokens)

run :: LoxState -> String -> IO LoxState
run state content = runStatements state $ stringToStatements content
  where runStatements :: LoxState -> Either String [Statement] -> IO LoxState
        runStatements _ (Left error) = putStrLnError ("There was a parsing error! " ++ error) >> return state
        runStatements s (Right statements) = foldl runStatement (return s) statements
        runStatement :: IO LoxState -> Statement -> IO LoxState
        runStatement state statement = state >>= ((flip evaluateStatement) statement) >>= resultToIOState
        resultToIOState :: (Show s) => Either (LoxState, (ProgramError s)) LoxState -> IO LoxState
        resultToIOState (Left (s, error)) = putStrLnError ("There was an error! " ++  (show error)) >> return s
        resultToIOState (Right newState) = return newState

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