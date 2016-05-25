{-# LANGUAGE OverloadedStrings #-}
        
-- | This module defines an abstract syntax tree for the C
-- language. The nodes in this tree may be annotated with additional
-- information, such as 'Span' information.
module AST where

import Text.PrettyPrint.ANSI.Leijen


indentLevel :: Int
indentLevel = 4


-- | C expressions with annotations.
data Expr a = Mult a (Expr a) (Expr a)
            | Div a (Expr a) (Expr a)
            | Mod a (Expr a) (Expr a)
            | Add a (Expr a) (Expr a)
            | Sub a (Expr a) (Expr a)
            | Neg a (Expr a)
            | Pos a (Expr a)
            | IntLit a Integer
            | Ident a String
            | Parens a (Expr a)
            deriving (Show)


-- | C statements with annotations.
data Stmnt a = If a (Expr a) (Stmnt a)
             | IfElse a (Expr a) (Stmnt a) (Stmnt a)
             | While a (Expr a) (Stmnt a)
             | DoWhile (Stmnt a) (Expr a)
             | For (Expr a) (Expr a) (Expr a) (Stmnt a)
             | Block a [Stmnt a]
             | ExprStmnt a (Expr a)
             deriving (Show)


instance Pretty (Expr a) where
  pretty (Mult _ a b) = pretty a <+> "*" <+> pretty b
  pretty (Div _ a b) = pretty a <+> "/" <+> pretty b
  pretty (Mod _ a b) = pretty a <+> "%" <+> pretty b
  pretty (Add _ a b) = pretty a <+> "+" <+> pretty b
  pretty (Sub _ a b) = pretty a <+> "-" <+> pretty b
  pretty (Neg _ a) = "-" <> pretty a
  pretty (Pos _ a) = "+" <> pretty a
  pretty (IntLit _ a) = pretty a
  pretty (Ident _ a) = pretty a
  pretty (Parens _ a) = "(" <> pretty a <> ")"


instance Pretty (Stmnt a) where
  pretty (If _ e s) =
    vcat ["if (" <> pretty e <> ")"
         , indent indentLevel (pretty s)
         ]
  pretty (IfElse _ e s1 s2) =
    vcat ["if (" <> pretty e <> ")"
         , indent indentLevel (pretty s1)
         , "else "
         , indent indentLevel (pretty s2)
         ]
  pretty (ExprStmnt _ e) = pretty e <> ";"
  pretty (Block _ s) = 
    vcat [ "{"
         , indent indentLevel (vcat (map pretty s))
         , "}" 
         ]
