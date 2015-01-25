--This module holds all the relevant functions for Recursive Descent Parsing.
--A big thanks to Martin Escardo for teaching me about parsing as part of the Functional Programming module.
--author: Yu-Yang Lin
module RecursiveParser where

import Data.Char
import AbstractSyntax
import Tokeniser

-- Syntax Parser (Syntactical Analysis)
syntacticalAnalysisR :: [Token] -> Expr
syntacticalAnalysisR [] = syntaxErrorR "Expected an Expression but found end of input"
syntacticalAnalysisR xs =
    let (expr, ys) = scanExpr xs
    in case ys of
        [] -> expr  -- Everything worked out, no left-over inputs
        _  -> syntaxErrorR("Expected end of input but found '" ++ show ys ++ "'")

-- Syntax Error Function
syntaxErrorR :: String -> a
syntaxErrorR xs = error ("***SYNTAX ERROR*** " ++ xs)

{-
Grammar Used for Recursive Descent Parsing:
    E ::= E + T | T
    T ::= T * F | F
    F ::= (E) | INT | ID
The scan functions are analogous to production rules.
-}

-- Scan for Expression Function
scanExpr, scanTerm, scanFactor :: [Token] -> (Expr, [Token])
scanExpr xs =
    let (expr, tail) = scanTerm xs   -- first scan for terms
    in case tail of                  -- then check if the tail begins with an Add operation
        OpT Add : tails_tail -> let (second_expr, tails_tails_tail) = scanExpr tails_tail
                                in (Op Add [expr, second_expr], tails_tails_tail)
        _                    -> (expr, tail) -- send result up

-- Scan for Term Function
scanTerm xs =
    let (expr, tail) = scanFactor xs -- first scan for factors
    in case tail of                  -- then check if the tail begins with an Mul operation
        OpT Mul : tails_tail -> let (second_expr, tails_tails_tail) = scanTerm tails_tail
                                in (Op Mul [expr, second_expr], tails_tails_tail)
        _                    -> (expr, tail) -- send result up

-- Scan for Factor Function
scanFactor (ConstantT n : xs) = (Constant n,  xs) -- Constant found, send result up
scanFactor (IdT n : xs) = (Id n,  xs)             -- Identifier found, send result up
scanFactor (OpenBracketT : xs) =                  -- OpenBracket found,
    let (expr, tail) = scanExpr xs                -- Restart scan for xs
    in (expr, scanCloseBracket tail)              -- look for a closing bracket in tail
scanFactor [] = syntaxErrorR("Unexpected end of input")      -- Met end of input unexpectedly
scanFactor xs = syntaxErrorR("Unexpected input " ++ show xs) -- Fell through all checks, nothing matched.

-- Scan for Closing Bracket Function
scanCloseBracket :: [Token] -> [Token]
scanCloseBracket []    = syntaxErrorR("Expected ')' but found end of input")
scanCloseBracket (x : xs)
    | x == CloseBracketT = xs -- Found a closing bracket, send result up.
    | otherwise          = syntaxErrorR("Expected ')' but found " ++ show(x:xs))

-- Parse Function, Composes the syntax and lexical analysis to produce a Parse Tree compatible with the Interpreter module
parseR :: String -> Expr                        --evaluates on the way as Haskell uses lazy evaluation.
parseR = syntacticalAnalysisR . lexicalAnalysis --ie: this should be able to parse an infinite string.
