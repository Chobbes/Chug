module Parser where

import AST
import Text.Parser.Expression
import Text.Parser.Token.Style
import Text.Parser.Token.Highlight
import Text.Parser.Token
import Text.Parser.Char
import Text.Trifecta
import Text.Trifecta.Combinators
import Control.Applicative
import qualified Data.HashSet as HS



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
cOperatorTable = [ pure AssocLeft <**> [binary "*" Mult, binary "/" Div, binary "%" Mod]
                 , pure AssocLeft <**> [binary "+" Add, binary "-" Sub]
                 ]
  where binary op f assoc = Infix (spannotate f <$> spanned (reservedOp op)) assoc
        prefix op f = Prefix (spannotate f <$> spanned (reservedOp op))
        postfix op f = Postfix (spannotate f <$> spanned (reservedOp op))


cTerm :: DeltaParsing m => m (Expr Span)
cTerm = spannotate1 IntLit <$> spanned natural

-- | Spanned to an annotation.
spannotate :: (Span -> b) -> Spanned a -> b
spannotate f (_ :~ span) = f span

-- | Spanned to an annotation.
spannotate1 :: (Span -> a -> b) -> Spanned a -> b
spannotate1 f (a :~ span) = f span a


reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve emptyOps
