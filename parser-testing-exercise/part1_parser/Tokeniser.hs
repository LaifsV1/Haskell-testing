--This module holds all functions relevant to lexical analysis of a string.
--This lexer is bulky, which is why I implemented another one for the second part of the exercise.
--One thing this lexer is good at, though, is outputting error messages. Though it uses error instead
--of proper error handling. ie, using Either with MonadPlus or a custom ErrorMonad.
--author: Yu-Yang Lin
module Tokeniser where

import Data.Char
import AbstractSyntax

-- Token data type
data Token = ConstantT Int      -- constant token for INT
           | OpT OpName         -- operation token for ADD and MUL
           | IdT Identifier     -- Identifier token for ID
           | OpenBracketT       -- "("
           | CloseBracketT      -- ")"
           deriving (Eq, Show)
           
-- Operation table for 'lookup' function
-- This was made to handle the event of multiple operations.
-- However, for this simple grammar, a lookup table adds pointless complexity.
operatorTable :: [(String, Token)]  
operatorTable = [("+" , OpT Add),("*" , OpT Mul)]

-- Operator Lookup Function (Uses Maybe monad to pass errors)
operatorLookup :: String -> Token   
operatorLookup xs = 
    case lookup xs operatorTable of 
        Nothing -> lexicalError("non-existent operator '" ++  xs ++ "'")
        Just t  -> t

-- Constant Getter Function (transforms the input list into a constant if possible)
getConstant :: String -> Token
getConstant (x:xs)
            | isDigit x = ConstantT (read(x:xs))
getConstant ('-':xs)
            | xs == []          = lexicalError("non-existent number '-'")
            | isDigit (head xs) = ConstantT (read('-':xs))
            | otherwise         = lexicalError("non-existent number '" ++  ('-':xs) ++ "'")

-- Function to check if it may be an identifier
isIdentifier :: Char -> Bool
isIdentifier x = isAlpha x || isDigit x || x == '_' -- only alpha-numeric and '_' allowed

-- Function to check if it may be a constant
isConstant :: Char -> Bool
isConstant x = isDigit x || x == '-'

-- Function to check if it may be an operator
isOperator :: Char -> Bool
isOperator x = not(isIdentifier x       -- not an identifier
                || isConstant x         -- not a constant
                || (elem x ['(', ')'])  -- not brackets
                || isSpace x)           -- not space

-- Lexical Error Function
lexicalError :: String -> a                   
lexicalError xs = error ("***LEXICAL ERROR*** " ++ xs)

-- String Tokenizer (Lexical Analysis)
lexicalAnalysis :: String -> [Token]
lexicalAnalysis [] = [] -- Base case (empty string)

lexicalAnalysis (x:xs) -- Ignore all spaces (whitespace, tab, end of line)
            | isSpace x = lexicalAnalysis xs

lexicalAnalysis ('(':xs) = OpenBracketT  : lexicalAnalysis xs -- open bracket
lexicalAnalysis (')':xs) = CloseBracketT : lexicalAnalysis xs -- close bracket

lexicalAnalysis (x:xs) -- Identifiers start with alphabetical character.
            | isAlpha x = let (ys, zs) = span isIdentifier xs
                          in IdT (x:ys) : lexicalAnalysis zs
                         
lexicalAnalysis (x:xs) -- Constants (Positive or Negative Integer)
            | isConstant x = let (ys, zs) = span isDigit xs
                             in getConstant (x:ys) : lexicalAnalysis zs
                          
lexicalAnalysis xs     -- Assume it is an operator (not ID, INT or SPACE)
            | otherwise = let (ys, zs) = span isOperator xs
                          in operatorLookup ys : lexicalAnalysis zs
