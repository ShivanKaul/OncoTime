module Lexer where

languageDef = 
    emptyDef{
           Token.commentLine     = "#"
           , Token.identStart = letter <|> char '_'
           ,Token.identLetter = alphaNum <|> char '_'
          , Token.reservedNames   = [

                                    ]
          , Token.reservedOpNames = []
    }


lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
stringLit = Token.stringLiteral lexer
integer  = Token.integer lexer -- parses an integer
float = Token.float lexer
semi      = Token.semi       lexer -- parses a semicolon
colon       =Token.colon       lexer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
parens = Token.parens lexer
dot = Token.dot lexer
--HAD TO CHANGE THIS
lexeme = Token.lexeme lexer

