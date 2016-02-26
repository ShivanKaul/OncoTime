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
import Control.Applicative (some) 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char

import Types
import PrettyPrinter
import Lexer

--use this Parser to test
testParser:: Parser TestProgram
testParser = 
    do
        whiteSpace
        --try testHeader <|>testUse <|> testGroups <|> try testComputation <|>  try testDocs 
        --testUse
        testDocs
--testProgram::Parser TestProgram


testHeader::Parser TestProgram
testHeader =
    do
        hdr <- header
        return $ TestHeader hdr

testDocs::Parser TestProgram
testDocs =
    do
        docs <- documentation
        return $TestDocs docs

testUse::Parser TestProgram
testUse = 
    do
        use <- many useList
        return $ TestUseFileList use

testGroups::Parser TestProgram
testGroups = 
    do
        grp <- many groups
        return $ TestGroupList grp

testComputation::Parser TestProgram
testComputation =
    do
        comp <- many computation
        return $ TestComputation comp

oncoParser:: Parser Program
oncoParser = 
    do
        hdr <- header
        doc <- documentation 
        use <- many useSection
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
        args <- parens $ arg `sepBy` comma
        return $ Header fname args

arg :: Parser Arg
arg = 
    do
        t <- groupType
        v <- var
        return $ Arg t v



--just gets the next string
var:: Parser Var
var = lexeme $
    do
        var <- some alphaNum
        return $ Var var

filename::Parser FileName
filename = lexeme $
    do
        fname <- many alphaNum
        return fname

documentation :: Parser Docs
documentation = lexeme $
    do  
        reserved "/*"
        doc <- stringLit
        reserved "*/"
        return $ Docs doc

groups::Parser GroupDefs
groups = lexeme $ 
    do
        reserved "group"
        grpType <- groupType
        v <- var
        reserved "="  
        --grpItem <- many groupItem
        grpItem <- some groupItem
        return $ Group grpType v grpItem

groupType::Parser GroupType
groupType = lexeme $
    do
        gt <- some alphaNum
        return $ GroupType gt

groupItem::Parser GroupItem
groupItem = try groupVal
        <|> try groupVar
        <|> try groupRange

groupVal::Parser GroupItem
groupVal = lexeme $
    do
        --gv <- many alphaNum
        gv <- some alphaNum
        dot
        fext <- some alphaNum
        --fext<- many alphaNum
        return $ GroupVal gv fext

groupVar::Parser GroupItem
groupVar =
    do
        gv <- var
        return $ GroupVar gv

groupRange::Parser GroupItem
groupRange = try (liftM GroupRange before) <|> try (liftM GroupRange after) <|> try (liftM GroupRange betw)

before::Parser RangeType
before =
    do
        reserved "before"
        pre <- some digit
        return $ Before $ read pre

after::Parser RangeType
after =
    do
        reserved "after"
        post <- some digit
        return $ Before $ read post

betw::Parser RangeType
betw =
    do
        pre <- some digit
        reserved "to"
        post <- some digit
        return $ Between (read pre) (read post)

computation::Parser Computation
computation = 
    try (liftM2 Foreach foreach ( curlies $ many computation)) 
    <|> try (liftM Table table) 
    <|> try (list) --STILL NEED TO DO
    <|> try (liftM Print prints) 
    <|> try (liftM Barchart barchart) 

foreach::Parser ForEachDef
foreach = 
    do
        forEachFilter <|> forEachTable <|> forEachSequence <|>forEachList

table::Parser TableAction
table = 
    do
        reserved "table"
        v <- var
        equal
        reserved "count"
        fn <- filterName
        reserved "by"
        fv <-filterVal
        return $ TableCount v fn fv


--NEEDS WORK
list::Parser Computation
list=
    do
        reserved "list"
        v <- var
        reserved "=" --equal --equal
        reserved "sequences"
        reserved "like"
        e <- seqList 
        return $ List v e

seqList::Parser [[SeqField]]
seqList= squares $ sepBy singleSequence bar
        
singleSequence::Parser [SeqField]
singleSequence = sepBy seqField arrow 

seqField::Parser SeqField
seqField = seqSingle <|> seqDisj <|> seqStar <|> seqNeg

seqSingle::Parser SeqField
seqSingle =
    do
        e <- event
        return $ Single e
seqStar::Parser SeqField
seqStar =
    do
        e <- seqField
        star
        return $ Star e
seqNeg::Parser SeqField
seqNeg =
    do
        e <- parens $ seqNot --TODO: Write this better
        return $ Neg e
seqNot::Parser Event
seqNot =
    do 
        reserved "not"
        event

seqDisj :: Parser SeqField
seqDisj = 
    do
        e <- curlies $ sepBy event comma
        return $ Disj e

event::Parser Event
event = try (liftM EAll eventName) <|> eSome
eSome::Parser Event
eSome = do
    e<-eventName
    param<-parens $ sepBy var comma
    return $ ESome e param

eventName :: Parser EventName
eventName =
    do
        ename <- identifier
        return $ ename 


prints:: Parser PrintAction
prints = try printvar <|> try printTimeLine <|> try printLength <|> try printFilters <|> printElement 

barchart::Parser Var 
barchart =  lexeme $
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
        return $ ForEachTable v1 v2

forEachSequence::Parser ForEachDef
forEachSequence =
    do
        reserved "foreach"
        reserved "sequence"
        v1 <- var
        reserved "like"
        e <- seqList
        return $ ForEachSequence v1 e

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
        reserved "length"
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
        v2 <- squares $ var
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
        choice $ [reserved "is", reserved "are"]
        filterDs <- many filterDefs
        return $ Filter fname filterDs

filterDefs :: Parser FilterDef
filterDefs = 
    do
        ffield <- identifier
        fval <- many filterVal
        return $ FilterDef (FilterField ffield) fval

useList :: Parser UseFile 
useList = lexeme $
    do 
        reserved "use"
        names <- sepBy grpFile comma 
        return $ UseManyFile names

--Not used
useSection::Parser UseFile
useSection = useFile <|> try useList

useFile :: Parser UseFile
useFile =
    do  
        reserved "use"
        --file <- some alphaNum
        --dot
        --string "grp"
        file <- grpFile
        return $ UseFile file

grpFile::Parser String
grpFile = lexeme $
    do
        file <- some alphaNum
        dot
        string "grp"
        return file
