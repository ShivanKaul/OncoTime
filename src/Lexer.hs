{-
OncoTime - Implementation of cancer-research domain-specific language as a project undertaken for
COMP 520 - Compiler Design in Winter 2016 at McGill University by

Shivan Kaul Sahib
Yusaira Khan
Brendan Games Gordon

The course was taught by Laurie Hendren.
 -}

module Lexer where

import System.FilePath
import System.Exit
import System.Environment
import System.IO
import qualified Data.Map as M
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
    emptyDef {
            Token.commentLine = "//"
            ,Token.identStart = letter <|> char '_' <?> "start of identifier"
            ,Token.identLetter = alphaNum <|> char '_' <?> "rest of identifier"
            ,Token.reservedNames = [
                "script",
                "/**",
                "*/",
                "is",
                "are",
                "foreach",
                "print",
                "group",
                "use",
                "to",
                "table",
                "count",
                "by",
                "element",
                "list",
                "timeline",
                "barchart",
                "of",
                "in",
                "sequences",
                "sequence",
                "member",
                "before",
                "length",
                "after",
                "=",
                "not",
                "Type",
                "Fields",
                "[",
                "]",
                "(",
                ")",
                "ToJoinOn",
                "JoinableFields"

            ]

            ,Token.reservedOpNames = []
    }


lexer = Token.makeTokenParser languageDef
curlies = Token.braces lexer
squares = Token.brackets lexer
angles = Token.angles lexer
parents = Token.parens lexer
dot = Token.dot lexer
comma = Token.comma lexer
identifier = (Token.identifier lexer) <?> "identifier" -- parses an identifier
reserved = Token.reserved lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
stringLit = Token.stringLiteral lexer
charLit = Token.charLiteral lexer
integer = Token.integer lexer -- parses an integer
float = Token.float lexer

colon = Token.colon lexer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
parens = Token.parens lexer
lexeme = Token.lexeme lexer
commaSep1OrMore = Token.commaSep1 lexer
symbol = Token.symbol lexer
bar = symbol "|"
arrow = symbol "->"
star = symbol "*"
equal = symbol "="
semi = symbol ";" <?> "newline" -- parses a semicolon
date_sep = symbol "-" <?> "Date separator \"-\""
