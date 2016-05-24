-- | This module defines an abstract syntax tree for the C
-- language. The nodes in this tree may be annotated with additional
-- information, such as 'Span' information.
module AST where


-- | C expressions with annotations.
data Expr a = Mult a (Expr a) (Expr a)
            | Div a (Expr a) (Expr a)
            | Mod a (Expr a) (Expr a)
            | Add a (Expr a) (Expr a)
            | Sub a (Expr a) (Expr a)
            | Neg a (Expr a)
            | IntLit a Integer
            | Ident a String
            deriving (Show)
