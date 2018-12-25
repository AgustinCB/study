{-# LANGUAGE ScopedTypeVariables #-}
module Hlox (scanTokens, evaluateStatement, parseStatement, Statement, ScanningResult, LoxState,
             ParsingExpressionResult, Token, isComment, ParsingExpressionStep, ProgramError, zeroState) where

import Interpreter
import Lexer
import Parser
import Types

isComment :: Token -> Bool
isComment (Token Comment _ _) = True
isComment _ = False
