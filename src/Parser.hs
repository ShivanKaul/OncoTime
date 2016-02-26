module Parser where

import Lexer
import System.Directory
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
-- import PrettyPrinter
import Lexer


verifyGroupFiles::[String]->IO ()
verifyGroupFiles providedList =
    do
        dirContents <- getDirectoryContents "."
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let listNames = map dropExtension grpFiles 
        if (sort listNames) == (sort providedList) then putStrLn "GroupFiles Exist" else fail "SOME GROUP FILES MISSING"
            -- -> putStrLn "ERROR" 
            --Right r -> putStrLn "All Group Files Exist"

--use this Parser to test
testParser :: Parser TestProgram
testParser = 
    do
        whiteSpace
        --try testHeader <|>testUse <|> testGroups <|> try testComputation <|>  try testDocs 
        testHeader 
        -- testUse
        -- testDocs
        -- testGroups
        -- testFilters
        -- testComputation

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

testFilters::Parser TestProgram
testFilters = 
    do
        filters <- many filters
        return $ TestFiltersList filters

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
        use <- many useList
        grp <- many groups
        filt <- many filters 
        comp <- manyComp
        return $ Program hdr doc use grp filt comp 


--IO to checkfilename
header:: Parser Header
header = 
    do
        reserved "script"
        fname <- filename
        args <- parens $ arg `sepBy` comma
        semi
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
        var <- identifier--some alphaNum
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
        grpItem <- curlies $ sepBy groupItem comma 
        semi
        return $ Group grpType v grpItem

groupType::Parser GroupType
groupType = lexeme $
    do
        gt <- some alphaNum
        return $ GroupType gt

groupItem::Parser GroupItem
groupItem = try groupRange
        <|> try groupVal
        <|> try groupVar

groupVar::Parser GroupItem
groupVar =
    do
        gv <- angles $ var
        return $ GroupVar gv

groupVal::Parser GroupItem
groupVal = lexeme $
    do
        gv <- some alphaNum
        return $ GroupVal gv

groupRange::Parser GroupItem
groupRange = try (liftM GroupRange betw) <|> try (liftM GroupRange before) <|> try (liftM GroupRange after)

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
        return $ After $ read post

betw::Parser RangeType
betw = lexeme $
    do
        pre <- lexeme $ some digit
        reserved "to"
        post <- lexeme $ some digit
        return $ Between (read pre) (read post)
manyComp ::Parser [Computation]
manyComp = 
    do 
        c <- curlies $ (optional semi) >> (many computation)
        semi
        return c

computation::Parser Computation
computation = 
    try (liftM2 Foreach foreach manyComp) 
    <|> try (liftM Table table) 
    <|> try (list)
    <|> try (liftM Print prints) 
    <|> try (liftM Barchart barchart) 

foreach::Parser ForEachDef
foreach = 
    do
        try(forEachFilter) <|> try(forEachTable) <|> try(forEachSequence) <|> try(forEachList)

table::Parser TableAction
table = 
    do
        reserved "table"
        v <- var
        reserved "="
        reserved "count"
        fn <- filterName
        reserved "by"
        fv <-filterVal
        semi
        return $ TableCount v fn fv

list::Parser Computation
list=
    do
        reserved "list"
        v <- var
        reserved "=" --equal --equal
        reserved "sequences"
        reserved "like"
        e <- seqList 
        semi
        return $ List v e

seqList::Parser [[SeqField]]
seqList= squares $ sepBy singleSequence bar
        
singleSequence::Parser [SeqField]
singleSequence = sepBy seqField arrow 

seqField::Parser SeqField
seqField = try(seqSingle) <|> try(seqDisj) <|> try(seqNeg) <|> try(seqStar) 

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
event =  try (eSome) <|> try (liftM EAll eventName) 
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
        semi
        return $ v


forEachFilter::Parser ForEachDef 
forEachFilter = 
    do
        reserved "foreach"
        f <- lexeme filterName
        v <- var
        semi
        return $ ForEachFilter f v


forEachTable::Parser ForEachDef 
forEachTable =
    do
        reserved "foreach"
        reserved "element"
        v1 <- var
        reserved "of"
        v2 <- var
        semi
        return $ ForEachTable v1 v2

forEachSequence::Parser ForEachDef
forEachSequence =
    do
        reserved "foreach"
        reserved "sequence"
        v1 <- var
        reserved "like"
        e <- seqList
        semi
        return $ ForEachSequence v1 e

forEachList::Parser ForEachDef 
forEachList = 
    do
        reserved "foreach"
        reserved "member"
        v1 <- var
        reserved "of"
        v2 <- var
        semi
        return $ ForEachList v1 v2

printvar::Parser PrintAction
printvar = 
    do
        reserved "print"
        v <- var
        semi
        return $ PrintVar v

printTimeLine::Parser PrintAction
printTimeLine =
    do
        reserved "print"
        reserved "timeline"
        reserved "of"
        v<-var
        semi
        return $ PrintTimeLine v


printLength::Parser PrintAction
printLength =
    do
        reserved "print"
        v<-var
        dot
        reserved "length"
        semi
        return $ PrintLength v 


printFilters::Parser PrintAction
printFilters =
    do
        reserved "print"
        filterList <- sepBy filterName comma
        v <- var
        semi
        return $ PrintFilters filterList v
        


printElement::Parser PrintAction
printElement = 
    do
        reserved "print"
        v1 <-var
        v2 <- squares $ var
        semi
        return $ PrintElement v1 v2

filterName::Parser FilterName
filterName = lexeme $
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
        fname <- lexeme $ identifier
        choice $ [reserved "is", reserved "are"]
        semi
        filterDs <- many filterDefs
        return $ Filter fname filterDs

filterDefs :: Parser FilterDef
filterDefs = 
    do
        ffield <- identifier
        colon
        fval <- sepBy filterVal comma 
        semi
        return $ FilterDef (FilterField ffield) fval

useList :: Parser UseFile 
useList = lexeme $
    do 
        reserved "use"
        names <- sepBy grpFile comma
        semi
        return $ UseManyFile names



--TypesMap First elemento
--How the Maps work:
--high level: fields: Population, Doctor, ID. Each of these has a map attached.
--i.e. FieldName, subFieldMap 
--That map is ANOTHER map of the possible fields it can have, and what they are allowed to have.
--

--data Conf = Conf (M.Map FieldName (SubField))
--data Conf = Conf String (M.Map String (SubField))

data Conf = Conf FieldName (M.Map SubFieldName (SubField)) deriving(Eq, Show)
type SubField = (AllowedType, [AllowedVal])


data TypesMap = TypesMap FieldName (M.Map AllowedType [AllowedVal])
type FieldName = String
type AllowedType = String
type AllowedVal = String
type SubFieldName = String

--SubFieldName (AllowedType, AllowedVal)
typeMapMaker::Parser (String, (String, [String])) 
typeMapMaker =
    do
        name <- stringLit 
        colon
        tup <- parens $ typeTuple
        return (name, tup) 

typeTuple::Parser (AllowedType, [AllowedVal])
typeTuple =
    do
        alType <- identifier
        comma
        alVals <- squares $ sepBy identifier comma
        return (alType, alVals)

confParser::Parser Conf
confParser =
    do
        whiteSpace
        fieldName <- identifier
        colon
        --reserved "["
        typeMapList <- squares $ sepBy typeMapMaker comma
        --reserved "]"
        semi
        --M.fromList typeMapList
        return $ Conf fieldName $ M.fromList typeMapList --maps
        --Each FieldName is part of a tuple between  between SubField:(AllowedType, AllowedVals)

grpFile::Parser String
grpFile = lexeme $
    do
        file <- some alphaNum
        dot
        string "grp"
        return file
