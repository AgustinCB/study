{-# LANGUAGE RankNTypes #-}

module Parser(runExercise) where

import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'
one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'
oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

type S = forall m. CharParsing m => m String
oneS :: S
oneS = string "1"

oneTwoS :: S
oneTwoS = string "12"

oneTwoThreeS :: S
oneTwoThreeS = string "123"

full :: Parser String
full = choice [ oneS, oneTwoS, oneTwoThreeS, stop ]

full' :: Parser Char
full' = choice [ char '1', char '1' >> char '2', char '1' >> char '2' >> char '3', stop ]

testParse :: Parser Char -> IO()
testParse p =
  print $ parseString p mempty "123"

testEOF :: Parser () -> IO()
testEOF p =
  print $ parseString p mempty "123"

testParse' :: Parser String -> IO ()
testParse' p =
  print $ parseString p mempty "123"

pNL :: [Char] -> IO()
pNL s = putStrLn ('\n' : s)

runExercise :: IO()
runExercise = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "one + eof:"
  testEOF $ one >> eof
  pNL "oneTwo + eof:"
  testEOF $ oneTwo >> eof
  pNL "string \"1\", \"12\", \"123\""
  testParse' full
  pNL "string with char \"1\", \"12\", \"123\""
  testParse full'
