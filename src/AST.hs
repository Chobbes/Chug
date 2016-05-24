{-# LANGUAGE OverloadedStrings #-}
        
-- | This module defines an abstract syntax tree for the C
-- language. The nodes in this tree may be annotated with additional
-- information, such as 'Span' information.
module AST where

import Text.PrettyPrint.ANSI.Leijen


-- | C expressions with annotations.
data Expr a = Mult a (Expr a) (Expr a)
            | Div a (Expr a) (Expr a)
            | Mod a (Expr a) (Expr a)
            | Add a (Expr a) (Expr a)
            | Sub a (Expr a) (Expr a)
            | Neg a (Expr a)
            | IntLit a Integer
            | Ident a String
            | Parens a (Expr a)
            deriving (Show)


instance Pretty (Expr a) where
  pretty (Mult _ a b) = pretty a <+> "*" <+> pretty b
  pretty (Div _ a b) = pretty a <+> "/" <+> pretty b
  pretty (Mod _ a b) = pretty a <+> "%" <+> pretty b
  pretty (Add _ a b) = pretty a <+> "+" <+> pretty b
  pretty (Sub _ a b) = pretty a <+> "-" <+> pretty b
  pretty (Neg _ a) = "-" <> pretty a
  pretty (IntLit _ a) = pretty a
  pretty (Ident _ a) = pretty a
  pretty (Parens _ a) = "(" <> pretty a <> ")"
