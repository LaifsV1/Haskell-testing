--This module holds all functions relevant to Predictive Recursive Descent Parsing.
--A big thanks to Ian Batten for teaching me about predictive parsers.
--author: Yu-Yang Lin
module PredictiveParser where

import Data.Char
import AbstractSyntax
import ConcreteSyntax
import Tokeniser

{-
Predictive Parser Table:
---------------------------------------------------------------
   |    (     |    a     |     )    |     +       |      *
---------------------------------------------------------------
E  | E::=T E' | E::=T E' |          |             |
E' |          |          |  E'::=e  | E'::=+ T E' |
T  | T::=F T' | T::=F T' |          |             |
T' |          |          |  T'::=e  |  T'::=e     | T'::=* F T'
F  | F::=(E)  | F::= a   |          |             |

e = Epsilon (empty string)
a = ID | INT

ProductionRule_Reference :: Int -> Production_Rule
    1  -> E ::= T E'
    2  -> E'::= + T E'
    3  ->     | Epsilon
    4  -> T ::= F T'
    5  -> T'::= * F T'
    6  ->     | Epsilon
    7  -> F ::= (E)
    8  ->     | ID
    9 ->      | INT

Predictive Parser Table Rows: pTable_X where X elem {E,E',T,T',F}
-}
pTable_E, pTable_E', pTable_T, pTable_T', pTable_F :: [(Token,Int)]
pTable_E  = [(OpenBracketT,  1)                            ] -- IdT and ConstantT are handled separately
pTable_E' = [(CloseBracketT, 3), (OpT Add, 2)              ]
pTable_T  = [(OpenBracketT,  4)                            ] -- IdT and ConstantT are handled separately
pTable_T' = [(CloseBracketT, 6), (OpT Add, 6), (OpT Mul, 5)]
pTable_F  = [(OpenBracketT,  7)                            ] -- IdT and ConstantT are handled separately

-- Syntax Parser (Syntactical Analysis)
syntacticalAnalysisP :: [Token] -> C_Tree
syntacticalAnalysisP [] = syntaxErrorP "Expected an Expression but found end of input"
syntacticalAnalysisP xs =
    let (expr, ys) = scanE xs
    in case ys of
        [] -> expr  -- Everything worked out, no left-over inputs
        _  -> syntaxErrorP("Expected end of input but found '" ++ show ys ++ "'")

-- Syntax Error Function
syntaxErrorP :: String -> a
syntaxErrorP xs = error ("***SYNTAX ERROR*** " ++ xs)

-- Production Rule Functions
production_1, production_2, production_4, production_5 :: [Token] -> (C_Tree, [Token])
-- RULE no.1 [E ::= T E']
production_1 (x:xs) = let (t, rem_T) = scanT (x:xs)           -- first scanT on input (x:xs)
                        in  let (e', rem_E') = scanE' rem_T   -- then scanE' on the remainder input of scanT
                            in (C_Expr t e', rem_E')          -- return syntax tree C_Expr t e' and the remainder rem_E'
-- RULE no.2 [E'::=+ T E']
production_2 (x:xs) = let (t, rem_T) = scanT (xs)             -- first scanT on input (xs)  (Discarding Add token)
                        in  let (e', rem_E') = scanE' rem_T   -- then scanE' on the remainder input of scanT
                            in (C_Add t e', rem_E')           -- return syntax tree C_Add t e' and the remainder rem_E'
-- RULE no.4 [T ::= F T']
production_4 (x:xs) = let (f, rem_F) = scanF (x:xs)           -- first scanF on input (x:xs)
                        in  let (t', rem_T') = scanT' rem_F   -- then scanT' on the remainder input of scanF
                            in (C_Term f t', rem_T')          -- return syntax tree C_Term f t' and the remainder scanT'
-- RULE no.5 [E'::=* F T']
production_5 (x:xs) = let (f, rem_F) = scanF (xs)             --   scanF on input (xs) (Discarding Mul token)
                        in  let (t', rem_T') = scanT' rem_F   -- then scanT' on the remainder input of scanF
                            in (C_Mul f t', rem_T')           -- return syntax tree C_Mul f t' and the remainder scanT'

-- Scan Functions for Predictive Parsing
-- This bit is ugly in my opinion. The Recursive Descent one was much easier to read.
scanE, scanE', scanT, scanT', scanF :: [Token] -> (C_Tree, [Token])
scanE []     = syntaxErrorP("Failed parsing F, reached end of input")
scanE (x:xs) =
    case x of
        ConstantT _ ->  production_1 (x:xs) -- To deal with Constants
        IdT       _ ->  production_1 (x:xs) -- To deal with Identifiers
        _     ->        -- Anything else in the lookup table
            case lookup x pTable_E of
                Nothing -> syntaxErrorP("Failed parsing E production rule at '"++ show x ++ "' with remainder " ++ show xs)
                Just _  ->  production_1 (x:xs)

scanE' []    = (C_Epsilon, []) -- NULLABLE
scanE' (x:xs) =
    case lookup x pTable_E' of  -- NOTE: doesn't need to deal with IdT or ConstantT, just stuff in the lookup table
        Nothing -> syntaxErrorP("Failed parsing E' production rule at '"++ show x ++ "' with remainder " ++ show xs)
        Just n  ->
            case n of
                -- RULE no.3 [E'::=Epsilon]
                3-> (C_Epsilon, x:xs)
                2-> production_2 (x:xs)

scanT []     = syntaxErrorP("Failed parsing T, reached end of input")
scanT (x:xs) =
    case x of
        ConstantT _ -> production_4 (x:xs)
        IdT       _ -> production_4 (x:xs)
        _     ->
            case lookup x pTable_T of --For the lookup table.
                Nothing -> syntaxErrorP("Failed parsing T production rule at '"++ show x ++ "' with remainder " ++ show xs)
                Just _  ->  production_4 (x:xs)

scanT' []    = (C_Epsilon, []) -- NULLABLE
scanT' (x:xs) =
    case lookup x pTable_T' of  -- NOTE: doesn't need to deal with IdT or ConstantT, just stuff in the lookup table
        Nothing -> syntaxErrorP("Failed parsing T' production rule at '"++ show x ++ "' with remainder " ++ show xs)
        Just n  ->
            case n of
                -- RULE no.6 [T'::=Epsilon]
                6-> (C_Epsilon, x:xs)
                -- RULE no.5 [E'::=* F T']
                5-> production_5 (x:xs)

scanF []     = syntaxErrorP("Failed parsing F, reached end of input")
scanF (x:xs) =
    case x of
        -- RULE no.9 [F ::= INT]
        ConstantT val -> (C_Const val, xs) -- return syntax tree C_Const val and the remainder input xs
        -- RULE no.8 [F ::= ID]
        IdT       id  -> (C_Id id, xs)     -- return syntax tree C_Id id and the remainder input xs
        -- RULE no.7 [F ::= (E)]
        _             ->
            case lookup x pTable_F of
                Nothing -> syntaxErrorP("Failed parsing E production rule at '"++ show x ++ "' with remainder " ++ show xs)
                Just n  ->  let (e,rem_E) = scanE (xs) in (e,scanCB rem_E) -- scanE on input xs (Discarding OpenBracketT)

scanCB :: [Token] -> [Token]    -- Looks of a Closing Bracket after doing "(E"
scanCB []    = syntaxErrorP("Expected ')' but found end of input")
scanCB (x : xs)
    | x == CloseBracketT = xs                          -- Found a closing bracket, send result up.
    | otherwise          = syntaxErrorP("Expected ')' but found " ++ show(x:xs))

-- Parse Function: Concrete Syntax, Uses Predictive Parsing to get the Concrete Syntax
parsePC :: String -> C_Tree
parsePC = syntacticalAnalysisP . lexicalAnalysis

-- Parse Function: Abstract Syntax, Uses the Concrete Syntax to get the Abstract Syntax
parsePA :: C_Tree -> Expr
parsePA C_Epsilon   = syntaxErrorP("Unexpected input while making Abstract Syntax Tree: C_Epsilon")
parsePA (C_Const n) = Constant n
parsePA (C_Id id)   = Id id
--NOTE: Term and Expr matching check the right tree if there is one, and parse it first.
--The result is an Op Node with a single input value (this is where making Op OpName take a list comes handy)
--This result is then pattern matched, and the parsed left tree is set as the first argument for Op OpName [l,r]
parsePA (C_Add t C_Epsilon) = Op Add [parsePA t]
parsePA (C_Add t e)         = let Op op xs = parsePA e in Op Add [Op op ((parsePA t):xs)]
parsePA (C_Mul f C_Epsilon) = Op Mul [parsePA f]
parsePA (C_Mul f t)         = let Op op xs = parsePA t in Op Mul [Op op ((parsePA f):xs)]
parsePA (C_Term e C_Epsilon) = parsePA e
parsePA (C_Term f t)         = let Op op xs = parsePA t in Op op ((parsePA f):xs)
parsePA (C_Expr t C_Epsilon) = parsePA t
parsePA (C_Expr t e)         = let Op op xs = parsePA e in Op op ((parsePA t):xs)

-- Parse Function, composes both parsing functions to get a Predictive Parse Tree compatible with the Interpreter module
parseP :: String -> Expr
parseP = parsePA . parsePC --Not sure if this allows lazy evaluation to benefit it properly as I have it here.
