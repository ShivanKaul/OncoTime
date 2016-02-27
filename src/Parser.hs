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
import Data.Char(isAlphaNum)
import TypeUtils
import Data.Char
import Debug.Trace
import Types
-- import PrettyPrinter
import Lexer


verifyGroupFiles::[String]->IO ()
verifyGroupFiles providedList =
    do
        dirContents <- getDirectoryContents "."
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let listNames = map dropExtension grpFiles 
        if (sort listNames) == (sort providedList) then putStrLn "GroupFiles Exist" else putStrLn "SOME GROUP FILES MISSING" >> exitFailure
            -- -> putStrLn "ERROR" 
            --Right r -> putStrLn "All Group Files Exist"

--use this Parser to test
testParser :: Parser TestProgram
testParser = 
    do
        whiteSpace
        --try testHeader <|>testUse <|> testGroups <|> try testComputation <|>  try testDocs 
        --testHeader 
        testUse
        -- testDocs
        --testGroups
        --testFilters
        -- testComputation
--
--
--Check tester
--
--

--testProgram::Parser TestProgram -}
testParserCheck ::[Conf]-> Parser TestProgram
testParserCheck c = 
    do
        whiteSpace
        --try testHeader <|>testUse <|> testGroups <|> try testComputation <|>  try testDocs 
        --testHeader 
        --testUse
        -- testDocs
        --testGroups
        testFiltersCheck c
        -- testComputation

testFiltersCheck::[Conf]->Parser TestProgram
testFiltersCheck c=
    do
        --filters <- many (filtersCheck c)
        filters <- many filters
        return $ TestFiltersList filters

{-
filtersCheck ::[Conf]-> Either LexError (Parser Filter)
filtersCheck c =
    do
        filterName <- lexeme $ identifier
        let a = read identifier
        case fieldExists $ a of
           False -> Left $ FieldNameError "ERROR: FIELD DOESN'T EXIST"
           True -> 
                do
                    choice $ [reserved "is", reserved "are"]
                    semi
                    filterDs <- lexeme $ some fD
                    Right $ Filter filterName filterDs -}
--
--
--
--

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
        filt <- many filters
        return $ TestFiltersList filt

testComputation::Parser TestProgram
testComputation =
    do
        comp <- many computation
        return $ TestComputation comp

oncoParser:: Parser Program
oncoParser =
    do
        whiteSpace
        hdr <- header
        doc <- documentation 
        use <- many useList
        grp <- many groups
        filt <- manyFilters
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

getFileName :: Parser String
getFileName = 
    do
        whiteSpace
        reserved "script"
        fname <- filename
        return fname

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
        doc <- docLiteral
        return $ Docs doc
docLiteral   = lexeme (
    do{ str <- between (symbol "/**")
        (symbol "*/" <?> "end of string")
        (many stringChar)
        ; return (foldr (maybe id (:)) "" str)
    }  <?> "literal string")

stringChar      =   do{ c <- stringLetter; return (Just c) }  <?> "string character"
stringLetter    = satisfy (\c -> (c /= '*'))



--stringEnd = satisfy (\c -> (c /= '/'))
wordChar = (satisfy (\c -> (isAlphaNum c) || (c=='-' )||(c=='_') ))


groups::Parser GroupDefs
groups = lexeme $ 
    do
        reserved "group"
        grpType <- groupType
        v <- var
        reserved "="
        grpItem <- curlies $ sepBy formattedGroup comma 
        semi
        return $ Group grpType v grpItem


formattedGroup::Parser GroupItem
formattedGroup = 
    do
      (optional semi)
      z<-groupItem
      (optional semi)  
      return z

groupType::Parser GroupType
groupType = lexeme $
    do
        gt <- some alphaNum
        return $ GroupType gt

groupItem::Parser GroupItem
groupItem = try groupRange
        <|> try groupValInt
        <|> try groupValString
        <|> try groupVar

groupVar::Parser GroupItem
groupVar =
    do
        gv <- angles $ var
        return $ GroupVar gv

groupValString::Parser GroupItem
groupValString = lexeme $
    do
        gv <- some wordChar
        return $ GroupValString gv

groupValInt::Parser GroupItem
groupValInt = lexeme $
    do
        gd <- some digit
        return $ GroupValInt $ read gd

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
        if (null c) 
            then trace ("WARNING: Computation list is empty.") semi
        else semi
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
seqList= lexeme $ squares $ sepBy singleSequence bar
        
singleSequence::Parser [SeqField]
singleSequence = 
    do 
        (optional semi)
        x <- sepBy seqField arrow
        (optional semi)
        return x

seqField::Parser SeqField
seqField = 
    do
        (optional semi)
        x <- try(seqSingle) <|> try(seqDisj) <|> try(seqNeg) <|> try(seqStar) 
        (optional semi)
        return x

seqSingle::Parser SeqField
seqSingle =
    do
        e <- event
        return $ Single e
seqStar :: Parser SeqField
seqStar = 
    do
        e <- curlies $ sepBy event comma
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
        reserved "in"
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
        reserved "of"
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
        f <- identifier 
        return f

filterVal::Parser FilterVal
filterVal = 
    do
        g <- groupItem
        return g

filters :: Parser Filter
filters =
    do
        filterName <- lexeme $ identifier
        choice $ [reserved "is", reserved "are"]
        semi
        filterDs <- lexeme $ some fD
        return $ Filter filterName filterDs

manyFilters :: Parser [Filter]
manyFilters =
    do
        f <- many filters
        return $ f

fD::Parser FilterDef
fD = try(filterDefs)

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
        --case verifyGroupFiles names of
          --  False -> fail
        let t = verifyGroupFiles names
        semi
        return $ UseFile names

grpFile::Parser String
grpFile = lexeme $
    do
        file <- some alphaNum
        dot
        string "grp"
        return file
