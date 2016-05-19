module Parser where

import Text.Parser.Expression
import Text.Parser.Token.Style
import Text.Parser.Token
import qualified Data.HashSet as HS



-- | Style for C comments.
cCommentStyle :: CommentStyle
cCommentStyle = CommentStyle "/*" "*/" "//" False


-- | Style for C identifiers, including reserved keywords.
cIdentifierStyle :: TokenParsing m => IdentifierStyle m
cIdentifierStyle = IdentifierStyle
  { _styleName = "C identifier style"
  , _styleStart = letter <|> char '_'
  , _styleLetter = alphaNum <|>  '_'
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


cOperatorTable :: OperatorTable m (Expr Span)
cOperatorTable = [ leftAssoc [binary "*" Mult, binary "/" Div, binary "%" Mod]
                 , leftAssoc [binary "+" Add, binary "-" Sub]
                 ]
  where leftAssoc = (<**>) (pure AssocLeft)
        rightAssoc = (<**>) (pure AssocRight)
        binary op f assoc = Infix (f <$> spanned (reservedOp op)) assoc
        prefix op f = Prefix (f <$> spanned (reservedOp op))
        postfix op f = Postfix (f <$> spanned (reservedOp op))


-- | Add a span annotation to an element.
annotate :: DeltaParsing m => (Span -> a -> s) -> m a -> m s
annotate spanify ma = createSpan <$> spanned ma
  where createSpan :: Spanned a -> a -> s
        createSpan (a :~ span) = spanify a span
