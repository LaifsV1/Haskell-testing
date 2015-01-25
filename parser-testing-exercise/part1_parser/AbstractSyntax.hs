--This module holds the AbstractSyntax Tree used for evaluating parses.
--author: Yu-Yang Lin
module AbstractSyntax where

type Identifier = String

data OpName = Add | Mul     -- + | *
            deriving (Eq, Show)

{-
    E ::= E + T | T
    T ::= T * F | F
    F ::= (E) | INT | ID
-}

--NOTE: This is the Abstract Syntax Tree, however, the grammar rules are NOT checked here.
--This is just the data structure, whether the rules will hold depends on the Parser.
data Expr = Op OpName [Expr]    -- E or T (+ or *)
          | Constant Int        -- F (INT)
          | Id Identifier       -- F (ID)
          deriving (Eq, Show)
          -- Passing a list of Expr as the argument for Op OpName was a design choice
          -- to make writing functions that build the tree easier. It also allowed me
          -- to pass Op OpName nodes with single or no arguments to older recursive
          -- calls. Moreover, using OpName instead of separate constructors made
          -- evaluating operations a bit easier to do.
