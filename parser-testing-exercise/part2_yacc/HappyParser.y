-- This is the grammar file for the second part of the exercise.
-- Happy is a parser generator system for Haskell, similar to yacc.
-- Happy is part of the Haskell Platform.
-- To generate the Haskell module, we use:
--    happy <filename>.y -i
-- "-i" produces <filename>.info, which contains detailed info, including states and reduction rules.
-- This is useful to debug the parser, but requires knowledge of the operation of shift-reduce parsers.
-- Author: Yu-Yang Lin
{
module Main where

import Data.Char
}
-- Haskell code goes in curly braces.

%name calc
%tokentype { Token }    -- we specify in this directive the structure for Tokens
%error { parseError }   -- we specify in this directive the error function used
-- Here, I could have requested monadic support: %monad { <type> } [ { <then> } { <return> } ]
-- I could have also requested a monadic lexer: %lexer { <lexer> } { <eof> }
-- This would allow threaded lexers as well as monadic parsing, allowing error handling with line and column numbers.

%token 
      int           { TokenInt $$ } -- $$ is a place-holder for the value of the token
      var           { TokenVar $$ } -- $$ is a place-holder for the value of the token
      '+'           { TokenPlus }
      '-'           { TokenMinus }
      '*'           { TokenTimes }
      '/'           { TokenDiv }
      '('           { TokenOB }
      ')'           { TokenCB }
      
%%  -- like yacc, this is here for no real reason.

Exp   : Exp '+' Term        { Plus $1 $3 }  -- where $n means the nth token
      | Exp '-' Term        { Minus $1 $3 }
      | Term                { Term $1 }

Term  : Term '*' Factor     { Times $1 $3 }
      | Term '/' Factor     { Div $1 $3 }
      | Factor              { Factor $1 }

Factor            
      : int                 { Int $1 }
      | var                 { Var $1 }
      | '(' Exp ')'         { Brack $2 }

-- Productions consists of:
--  non-terminals : expansion1  { associated Haskell code }
--                | expansion2  { associated Haskell code }

-- and it all means: n   : t_1 ... t_n   { E }
-- For monadic parsers: n  :  t_1 ... t_n  {% <expr> }

-- The parser finds the symbols t_1 ... t_n from the token stream,
-- constructs the symbol 'n' and gives it the value 'E'.
-- We refer to the symbols t_1 ... t_n using $1 ... $n

-- all parsers need the following code (in curly braces):
{

-- a polymorphic error function on type 'a'. Usually, this means it calls error.
-- we specified in the '%error' directive that the function called is parseError.
parseError :: [Token] -> a
parseError t = error("Parse error while parsing "++(show t))

-- declaring the data type that holds a parsed expression
data Exp  
      = Plus Exp Term 
      | Minus Exp Term 
      | Term Term
      deriving Show

data Term 
      = Times Term Factor 
      | Div Term Factor 
      | Factor Factor
      deriving Show

data Factor 
      = Int Int 
      | Var String 
      | Brack Exp
      deriving Show

-- declaring the data type for tokens
data Token
      = TokenInt Int
      | TokenVar String
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
 deriving Show
 
-- declaring a simple lexer for the token data structure 
-- I could have used Alex, Haskell's lex equivalent.
-- However, for this grammar, a lex file would be
-- larger than writing a simple lexer.
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer xs       = lexicalError ("unexpected input " ++ show xs)

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      (var,rest)   -> TokenVar var : lexer rest

-- lexical error function
lexicalError :: String -> a                   
lexicalError xs = error ("***LEXICAL ERROR*** " ++ xs)

-- the parse function
parse :: String -> Exp
parse = calc . lexer

main = getContents >>= print . calc . lexer
}
