module Parser where

import System.FilePath
import System.Exit
import System.Environment
import System.IO
import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Tokeno
import Data.Char

import Types
import PrettyPrinter
import Lexer

{--

oncoParser:: Parser Program

oncoParser = 
    do
        whiteSpace
        hdr <- header
        docs <- many groups
        use <- 
        group <-
        filter <- 
        comp <-
        return $ Program hdr docs use group filter comp -}

--IO to checkfilename
header:: Parser Header
header = 
    do 
        reserved "script"
        fname <- filename
        args <- parens $ sepBy var comma 
        return $ Header fname args

--just gets the next string
var:: Parser Var
var =
    do
    var <- many  alphaNum
    return var

filename::Parser FileName
filename =
    do 
        fname <- many alphaNum
        return fname


groups::Parser GroupDefs
groups =
    do
        grpType <- groupType
        v <- var
        grpItem <- many groupItem
        return $ Group grpType v grpItem 

groupType::Parser GroupType
groupType =
    do
        gt <- many  alphaNum
        return gt

groupItem::Parser GroupItem 
groupItem = try groupVal 
        <|> try groupVar
        <|> try groupRange

groupVal::Parser GroupItem
groupVal = 
    do
        gv <- many  alphaNum
        return $ GroupVal gv

groupVar::Parser GroupItem
groupVar =
    do
        gv <- many  alphaNum
        return $ GroupVal gv

groupRange::Parser GroupItem
groupRange = try (liftM GroupRange before) <|> try (liftM GroupRange after) <|> try (liftM GroupRange betw)

before::Parser RangeType
before =
    do
        reserved "before"
        pre <- many digit
        return $ Before $ read pre

after::Parser RangeType
after = 
    do
        reserved "after"
        post <- many digit
        return $ Before $ read post

betw::Parser RangeType
betw =
    do
        pre <- many digit
        reserved "to"
        post <- many digit
        return $ Between (read pre) (read post)

