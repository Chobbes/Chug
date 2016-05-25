module Parser where

import AST
import Text.Parser.Expression
import Text.Parser.Token.Style
import Text.Parser.Token.Highlight
import Text.Parser.Token
import Text.Parser.Char
import Text.Trifecta
import Control.Applicative
import qualified Data.HashSet as HS
import Data.Semigroup ((<>))
import Data.Foldable (asum)


-- | Style for C comments.
cCommentStyle :: CommentStyle
cCommentStyle = CommentStyle "/*" "*/" "//" False


-- | Style for C identifiers, including reserved keywords.
cIdentifierStyle :: TokenParsing m => IdentifierStyle m
cIdentifierStyle = IdentifierStyle
  { _styleName = "C identifier style"
  , _styleStart = letter <|> char '_'
  , _styleLetter = alphaNum <|> char '_'
  , _styleReserved = HS.fromList cKeywords
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }


-- | All of the keywords in the C99 standard in no particular order.
cKeywords :: [String]
cKeywords = [ "auto", "enum", "restrict", "unsigned"
            , "break", "extern", "return", "void"
            , "case", "float", "short", "volatile"
            , "char", "for", "signed", "while"
            , "const", "goto", "sizeof", "_Bool"
            , "continue", "if", "static", "_Complex"
            , "default", "inline", "struct", "_Imaginary"
            , "do", "int", "switch"
            , "double", "long", "typedef"
            , "else", "register", "union"
            ]


cExpression :: DeltaParsing m => m (Expr Span)
cExpression = buildExpressionParser cOperatorTable cTerm
            <?> "Expression"


cOperatorTable :: (DeltaParsing m, Applicative m) => OperatorTable m (Expr Span)
cOperatorTable = [ [prefix "-" Neg, prefix "+" Pos]
                 , pure AssocLeft <**> [binary "*" Mult, binary "/" Div, binary "%" Mod]
                 , pure AssocLeft <**> [binary "+" Add, binary "-" Sub]
                 ]
  where binary op f assoc = Infix (spannotate f <$> spanned (reservedOp op)) assoc
        prefix op f = Prefix (spannotate f <$> spanned (reservedOp op))
        -- postfix op f = Postfix (spannotate f <$> spanned (reservedOp op))


cTerm :: DeltaParsing m => m (Expr Span)
cTerm = ((spannotate1 IntLit <$> spanned natural) <?> "IntLit")
      <|> ((spannotate1 Parens <$> spanned (parens cExpression)) <?> "Parens")

-- | Spanned to an annotation.
spannotate :: (Span -> b) -> Spanned a -> b
spannotate f (_ :~ s) = f s

-- | Spanned to an annotation.
spannotate1 :: (Span -> a -> b) -> Spanned a -> b
spannotate1 f (a :~ s) = f s a

-- | Spanned to annotation.
spannotate2 :: (Span -> a -> b -> c) -> Spanned a -> Spanned b -> c
spannotate2 f (a :~ spanA) (b :~ spanB) = f (spanA <> spanB) a b

-- | Spanned to annotation.
spannotate3 :: (Span -> a -> b -> c -> d) -> Spanned a -> Spanned b -> Spanned c -> d
spannotate3 f (a :~ spanA) (b :~ spanB) (c :~ spanC) = f (spanA <> spanB <> spanC) a b c


reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve emptyOps


reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = reserve cIdentifierStyle

-- | C statements with annotations.
{-
data Stmnt a = If a (Expr a) (Stmnt a)
             | IfElse a (Expr a) (Stmnt a) (Stmnt a)
             | While a (Expr a) (Stmnt a)
             | DoWhile (Stmnt a) (Expr a)
             | For (Expr a) (Expr a) (Expr a) (Stmnt a)
             | Block a [Stmnt a]
             deriving (Show)
-}


cStatement :: (TokenParsing m, DeltaParsing m) => m (Stmnt Span)
cStatement = 
  asum [ try (ifElseStatement) <|> ifStatement
       , expressionStatement
       , blockStatement
       ]


ifStatement :: (TokenParsing m, DeltaParsing m) => m (Stmnt Span)
ifStatement = (spannotate2 If)
    <$> spanned (reserved "if" *> whiteSpace *> parens cExpression <* whiteSpace)
    <*> spanned (cStatement <* whiteSpace)
    <?> "if statement"


ifElseStatement :: (TokenParsing m, DeltaParsing m) => m (Stmnt Span)
ifElseStatement = (spannotate3 IfElse)
    <$> spanned (reserved "if" *> whiteSpace *> parens cExpression <* whiteSpace)
    <*> spanned (cStatement <* whiteSpace)
    <*> spanned (reserved "else" *> whiteSpace *> cStatement <* whiteSpace)
    <?> "if else statement"


expressionStatement :: (TokenParsing m, DeltaParsing m) => m (Stmnt Span)
expressionStatement = spannotate1 ExprStmnt
    <$> (spanned $ cExpression <* string ";" <* whiteSpace)
    <?> "expression statement"


blockStatement :: (TokenParsing m, DeltaParsing m) => m (Stmnt Span)
blockStatement = spannotate1 Block <$> (spanned $ braces (many cStatement))
