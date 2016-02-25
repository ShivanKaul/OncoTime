module Parser where

import Lexer
import System.FilePath
import Types
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
import qualified Text.ParserCombinators.Parsec.Token as Tokeno
import Data.Char

import Types
import PrettyPrinter
import Lexer


oncoParser:: Parser Program
oncoParser = 
    do
        whiteSpace
        hdr <- header
        doc <-documentation 
        use <- useList 
        grp <- many groups
        filt <- many filters 
        comp <- many computation
        return $ Program hdr doc use grp filt comp 


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

documentation :: Parser Docs
documentation =
    do  reserved "/*"
        doc <- stringLit
        reserved "*/"
        return doc

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

computation::Parser Computation
computation = try (liftM2 Foreach foreach (many computation)) <|> try (liftM Table table) <|> try (liftM Sequence sequ) <|> try (liftM Print prints) <|> try (liftM Barchart barchart) 

foreach::Parser ForEachDef
foreach = forEachFilter <|> forEachTable <|> forEachSequence <|> forEachList

table::Parser TableAction
table = 
    do
        v <- var
        fn <- filterName
        fv <-filterVal
        return $ TableCount v fn fv


--NEEDS WORK
sequ::Parser SeqAction
sequ=
    do
        v <- var
        e <- many stringLit --NEEdS WORK
        return $ Seq v e

prints:: Parser PrintAction
prints = printvar <|> printTimeLine <|> printLength <|> printFilters <|> printElement 

barchart::Parser Var 
barchart = 
    do
        reserved "barchart"
        v <- var
        return $ v


forEachFilter::Parser ForEachDef 
forEachFilter = 
    do
        reserved "foreach"
        f <- filterName
        v <- var
        return $ ForEachFilter f v


forEachTable::Parser ForEachDef 
forEachTable =
    do
        reserved "foreach"
        reserved "element"
        v1 <- var
        reserved "of"
        v2 <- var
        return $ ForEachFilter v1 v2

forEachSequence::Parser ForEachDef
forEachSequence =
    do
        reserved "foreach"
        reserved "member"
        v1 <- var
        reserved "in"
        v2 <- var
        return $ ForEachSequence v1 v2

forEachList::Parser ForEachDef 
forEachList = 
    do
        reserved "foreach"
        reserved "element"
        v1 <- var
        reserved "of"
        v2 <- var
        return $ ForEachList v1 v2

printvar::Parser PrintAction
printvar = 
    do
        reserved "print"
        v <- var
        return $ PrintVar v

printTimeLine::Parser PrintAction
printTimeLine =
    do
        reserved "print"
        reserved "timeline"
        reserved "of"
        v<-var
        return $ PrintTimeLine v


printLength::Parser PrintAction
printLength =
    do
        reserved "print"
        v<-var
        dot
        reserved"length"
        return $ PrintLength v 


printFilters::Parser PrintAction
printFilters =
    do
        reserved "print"
        filterList <- sepBy filterName comma
        v <- var
        return $ PrintFilters filterList v
        


printElement::Parser PrintAction
printElement = 
    do
        reserved "print"
        v1 <-var
        v2 <- braces $ var
        return $ PrintElement v1 v2

filterName::Parser FilterName
filterName =
    do
        f <- many alphaNum 
        return f

filterVal::Parser FilterVal
filterVal = 
    do
        g <- groupItem
        return g

filters :: Parser Filter
filters =
    do
        fname <- identifier
        filterDs <- many filterDefs
        return $ Filter fname filterDs

filterDefs :: Parser FilterDef
filterDefs = 
    do
        ffield <- identifier
        fval <- many filterVal
        return $ FilterDef ffield fval


useList :: Parser UseFileList 
useList =
    do  reserved "use"
        names <- sepBy useFile comma 
        return names

useFile :: Parser UseFile
useFile =
    do  file <- many alphaNum
        return file
