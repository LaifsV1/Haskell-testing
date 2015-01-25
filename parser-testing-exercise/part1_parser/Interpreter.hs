--This module serves as the Interpreter to access to all functions involved in Parsing.
--author: Yu-Yang Lin
module Interpreter where

import Control.Monad
import Tokeniser
import AbstractSyntax
import ConcreteSyntax
import RecursiveParser
import PredictiveParser

-- eval function will return mzero if it can't evaluate the arithmetic expression
-- As there is only one possible error, I decided to use MonadPlus.
-- This means this doesn't really handle error message properly. Instead, it has a hard-coded one.
eval :: MonadPlus m => Expr -> m Integer
eval (Constant n)    = return (toInteger n)
eval (Op Add [e,d])  = eval e >>= (\x -> eval d >>= (\y -> return((toInteger x) + (toInteger y)))) 
eval (Op Mul [e,d])  = eval e >>= (\x -> eval d >>= (\y -> return((toInteger x) * (toInteger y)))) 
eval (Id id)         = mzero

-- function that creates a string of n a's added/multiplied together. ie: a1 + a2 + a3 + .. + aN, where a1 = a2 = aN.
createSum, createMul, createBrackets :: Int -> Int -> String
createSum a n = foldr (\x -> ((show (x) ++ "+") ++)) (show a) (take (n-1) (cycle [a]))
createMul a n = foldr (\x -> ((show (x) ++ "*") ++)) (show a) (take (n-1) (cycle [a]))
createBrackets a n = (createString "(" n) ++ (show a) ++ (createString ")" n)

-- function that repeats any given string n-times.
createString :: String -> Int -> String
createString s n = (foldr (\x -> ((x ++ s) ++)) (s) (take (n-1) (cycle [s])))

-- runR function for Recursive Descent Parsing - will print a run-time error if eval failed
runR :: String -> IO()
runR x = let parsed = parseR x
        in case eval parsed of
            Nothing -> putStrLn ("***RUN-TIME ERROR*** Expected INT but got ID in ") >> putStrLn ("'"++show parsed++"'") 
            Just y  -> putStrLn (show y)

-- runR function for Predictive Recursive Descent Parsing - will print a run-time error if eval failed
runP :: String -> IO()
runP x = let parsed = parseP x
        in case eval parsed of
            Nothing -> putStrLn ("***RUN-TIME ERROR*** Expected INT but got ID in ") >> putStrLn ("'"++show parsed++"'") 
            Just y  -> putStrLn (show y)

-- function to test the parsers without output.
testR,testP,testPC :: String -> Int
testR x = if (parseR x) == Constant 1 then 0 else 1
testP x = if (parseP x) == Constant 1 then 0 else 1
testPC x = if (parsePC x) == C_Const 1 then 0 else 1
