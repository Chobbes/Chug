{-# LANGUAGE OverloadedStrings #-}
        
-- | This module defines an abstract syntax tree for the C
-- language. The nodes in this tree may be annotated with additional
-- information, such as 'Span' information.
module AST where

import Text.PrettyPrint.ANSI.Leijen


indentLevel :: Int
indentLevel = 4


-- | C expressions with annotations.
data Expr a b = Mult a (Expr a b) (Expr a b) b
              | Div a (Expr a b) (Expr a b) b
              | Mod a (Expr a b) (Expr a b) b
              | Add a (Expr a b) (Expr a b) b
              | Sub a (Expr a b) (Expr a b) b
              | Neg a (Expr a b) b
              | Pos a (Expr a b) b
              | IntLit a Integer b
              | Ident a String b
              | Parens a (Expr a b) b
              deriving (Show)


-- | C statements with annotations.
data Stmnt a b = If a (Expr a b) (Stmnt a b) b
               | IfElse a (Expr a b) (Stmnt a b) (Stmnt a b) b
               | While a (Expr a b) (Stmnt a b) b
               | DoWhile a (Stmnt a b) (Expr a b) b
               | For a (Expr a b) (Expr a b) (Expr a b) (Stmnt a b) b
               | Block a [Stmnt a b] b
               | ExprStmnt a b (Expr a b) b
               deriving (Show)


instance Pretty (Expr a b) where
  pretty (Mult _ a b _) = pretty a <+> "*" <+> pretty b
  pretty (Div _ a b _) = pretty a <+> "/" <+> pretty b
  pretty (Mod _ a b _) = pretty a <+> "%" <+> pretty b
  pretty (Add _ a b _) = pretty a <+> "+" <+> pretty b
  pretty (Sub _ a b _) = pretty a <+> "-" <+> pretty b
  pretty (Neg _ a _) = "-" <> pretty a
  pretty (Pos _ a _) = "+" <> pretty a
  pretty (IntLit _ a _) = pretty a
  pretty (Ident _ a _) = pretty a
  pretty (Parens _ a _) = "(" <> pretty a <> ")"


instance Pretty (Stmnt a b) where
  pretty (If _ e s _) =
    vcat ["if (" <> pretty e <> ")"
         , indent indentLevel (pretty s)
         ]
         
  pretty (IfElse _ e s1 s2) =
    vcat ["if (" <> pretty e <> ")"
         , indent indentLevel (pretty s1)
         , "else "
         , indent indentLevel (pretty s2)
         ]

  pretty (While _ e s) =
    vcat ["while (" <> pretty e <> ")"
         , indent indentLevel (pretty s)
         ]

  pretty (DoWhile _ s e) =
    vcat ["do"
         , indent indentLevel (pretty s)
         , "while (" <> pretty e <> ")"
         ]

  pretty (For _ b m e s) =
      vcat ["for (" <> pretty b <> "; " <> pretty m <> "; " <> pretty e <> ")"
           , indent indentLevel (pretty s)
           ]

  pretty (ExprStmnt _ e) = pretty e <> ";"

  pretty (Block _ s) = 
    vcat [ "{"
         , indent indentLevel (vcat (map pretty s))
         , "}" 
         ]
