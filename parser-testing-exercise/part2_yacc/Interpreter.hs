--This module serves as the Interpreter to access to all functions involved in Parsing.
--author: Yu-Yang Lin
module Interpreter where

import Control.Monad
import Main

-- eval function, uses Either to handle errors, Left String if the arithmetic expression can't be evaluated
-- Using MonadPlus might have made dealing with division by zero easier as it allows using guard().
-- However, I wanted to handle error properly, so I used Either. I could have used Either String (m Int),
-- but I didn't want to handle monads inside Either as it would make binding messy and difficult to read.
eval :: Exp -> Either String Integer
eval (Plus  e t) = do
                    x <- eval  e
                    y <- evalT t
                    return $ (toInteger x)+(toInteger y)
eval (Minus e t) = do
                    x <- eval  e
                    y <- evalT t
                    return $ (toInteger x)-(toInteger y)
eval (Term  t  ) = evalT t

-- evalT function for terms, if division by zero, Left error_message is returned
evalT :: Term -> Either String Integer
evalT (Times  t f) = do
                        x <- evalT t
                        y <- evalF f
                        return $ (toInteger x)*(toInteger y)
evalT (Div    t f) = do
                        x <- evalT t
                        y <- evalF f
                        case y of
                            0 -> Left ("Division by zero for "++show(Div t f))
                            _ -> return $ (toInteger x)`div`(toInteger y)
evalT (Factor f  ) = evalF f

-- evalF for factors, if variable involved, Left error_message is returned
evalF :: Factor -> Either String Integer
evalF (Int   i) = return $ toInteger i
evalF (Brack e) = eval e
evalF (Var   s) = Left ("Couldn't process variable "++show(s))

-- function that creates a string of n a's added/multiplied together. ie: a1 + a2 + a3 + .. + aN, where a1 = a2 = aN.
createSum, createMul, createBrackets :: Int -> Int -> String
createSum a n = foldr (\x -> ((show (x) ++ "+") ++)) (show a) (take (n-1) (cycle [a]))
createMul a n = foldr (\x -> ((show (x) ++ "*") ++)) (show a) (take (n-1) (cycle [a]))
createBrackets a n = (createString "(" n) ++ (show a) ++ (createString ")" n)

-- function that repeats any given string n-times.
createString :: String -> Int -> String
createString s n = (foldr (\x -> ((x ++ s) ++)) (s) (take (n-1) (cycle [s])))

-- runR function for Recursive Descent Parsing - will print a run-time error if eval failed
run :: String -> IO()
run x = let parsed = parse x
         in case eval parsed of
            Left  x -> putStrLn ("***RUN-TIME ERROR***") >> putStrLn (x)
            Right y  -> putStrLn (show y)
