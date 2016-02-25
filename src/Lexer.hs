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
            ,Token.identStart = letter <|> char '_'
            ,Token.identLetter = alphaNum <|> char '_'
            ,Token.reservedNames = [
                "script",
                "/*",
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
                "end",
                "->",
                "timeline",
                "barchart",
                "of",
                "sequences",
                "sequence",         
                "member",
                "before",
                "length",
                "after"
            ]

            ,Token.reservedOpNames = []
    }


lexer = Token.makeTokenParser languageDef
braces = Token.braces lexer
angles = Token.angles lexer
parents = Token.parens lexer
dot = Token.dot lexer
comma = Token.comma lexer
identifier = Token.identifier lexer -- parses an identifier
reserved = Token.reserved lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
stringLit = Token.stringLiteral lexer
integer = Token.integer lexer -- parses an integer
float = Token.float lexer
semi = Token.semi lexer -- parses a semicolon
colon = Token.colon       lexer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
parens = Token.parens lexer
lexeme = Token.lexeme lexer

