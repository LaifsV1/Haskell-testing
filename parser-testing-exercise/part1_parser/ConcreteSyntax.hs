--This module holds the Concrete Syntax Tree used for Predictive Recursive Descent Parsing.
--author: Yu-Yang Lin
module ConcreteSyntax where

import AbstractSyntax

{-
This module is used in the PredictiveParser module.
It holds the concrete syntax used to build the parse tree.

Concrete Syntax Grammar:

E ::= T E'
E'::= + T E' | Epsilon
T ::= F T'
T'::= * F T' | Epsilon
F ::= (E) | ID | INT

-}

--NOTE: This is the Concrete Syntax Tree, however, the grammar rules are NOT checked here.
--This is just the data structure, whether the rules will hold depends on the Parser.
data C_Tree = C_Expr C_Tree C_Tree              --Expressions     (E ::= T E')
            | C_Add  C_Tree C_Tree              --Additions       (E'::= + T E' | Epsilon)
            | C_Term C_Tree C_Tree              --Terms           (T ::= F T')
            | C_Mul  C_Tree C_Tree              --Multiplications (T'::= * F T' | Epsilon)
            | C_Const Int | C_Id Identifier     --Factors         (F ::= (E) | ID | INT)
            | C_Epsilon                         --Epsilon for E' and T'
            deriving (Eq, Show)
            -- NOTE: (E) doesn't need to be specified as it is already in C_Tree by definition
            -- Not making the constructors take C_Tree lists instead of separate arguments
            -- might have been a design error. It made building the tree more difficult
            -- and didn't allow passing around empty Nodes for past recursive calls.
            -- Another design error was not combining C_Add and C_Mull into a single C_Op.
            -- It made building the tree require more cases.
