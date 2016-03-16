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
testParserCheck ::Config-> Parser TestProgram
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

testFiltersCheck::Config->Parser TestProgram
testFiltersCheck c=
    do
        --filters <- many (filtersCheck c)
        filters <- many filters
        return $ TestFiltersList filters

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

getScriptName :: Parser String
getScriptName =
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
        fname <- some fChar
        return fname

documentation :: Parser Docs
documentation = lexeme $
    do
        doc <- docLiteral
        return $ Docs doc

docLiteral :: Parser String
docLiteral   = lexeme (
    do{ str <- between (symbol "/*" <?> "Start of Documentation String (/*)")
        (symbol "*/" <?> "end of Documentation String (*/)")
        (many docChar)
        ; return (foldr (maybe id (:)) "" str)
    }  <?> "Documentation String")

docChar :: Parser (Maybe Char)
docChar      =   do{ c <- validDocChar; return (Just c) }  <?> "Documentation string character"

validDocChar :: Parser Char
validDocChar    =
    do
        isNextValid <- isStarSlash
        --satisfy (\c -> (c /= '*'))
        satisfy (\c -> not isNextValid)


isStarSlash :: Parser Bool
isStarSlash =
    do
        char <- lookAhead $ count 2 anyChar
        return (char == "*/")



wordChar :: Parser Char
wordChar = (satisfy (\c -> (c=='_') || (isAlphaNum c) ))

fChar :: Parser Char
fChar = (satisfy (\c -> (c=='.' ) || (isAlphaNum c)))

manyGroups :: Parser [GroupDefs]
manyGroups =
    do
        grps <- many groups
        return grps

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
groupItem = try groupValDate
        <|> try groupRange
        <|> try groupValString
        <|> try groupVar

groupVar :: Parser GroupItem
groupVar =
    do
        gv <- angles $ var
        return $ GroupVar gv

groupValString::Parser GroupItem
groupValString = lexeme $
    do
        gv <- some wordChar
        return $ GroupValString gv

groupValDate::Parser GroupItem
groupValDate = lexeme (
    do {
        y <- some digit;
        date_sep;
        m <- some digit;
        date_sep;
        d <- some digit;
        return $ GroupDate (read y) (read m) (read d)} <?> "Date")

groupRange::Parser GroupItem
groupRange = try (liftM GroupRange betw) <|> try (liftM GroupRange before) <|> try (liftM GroupRange after)  <|> try (liftM GroupRange single) <?> "Number or Range"

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


single::Parser RangeType
single = lexeme $
    do
        gd <- some digit
        return $ SingleInt $ read gd


manyComp ::Parser [Computation]
manyComp = lexeme(
    do  {
        c <- between (symbol "{" <?> "Start of Computation Block \"{\"") 
        (symbol "}" <?> "End of computation block \"}\"") $
        (optional semi) >> (many computation);
        if (null c)
            then trace ("WARNING: Computation list is empty.") (optional semi)
        else (optional semi);
        return c}<?> "Computation Block \"{}\"")

singleComp :: Parser [Computation]
singleComp = lexeme( 
    do { 
    c <- computation;
    return [c] } <?> "Single Line Computation"
    )

computation::Parser Computation
computation =
    try (liftM2 Foreach foreach  manyComp {-(try-} {- <|> singleComp)-})
    <|> try (liftM Table table)
    <|> try (list)
    <|> try (liftM Print prints)
    <|> try (liftM Barchart barchart)

table::Parser TableAction
table = lexeme (
    do {
        reserved "table";
        v <- var;
        reserved "=";
        reserved "count";
        fn <- filterName;
        reserved "by";
        fv <-filterVal;
        semi;
        return $ TableCount v fn fv}<?>"Table Statement")

list::Parser Computation
list = lexeme (
    do {
        reserved "list";
        v <- var;
        reserved "="; --equal --equal
        reserved "sequences";
        reserved "like";
        e <- seqList;
        semi;
        return $ List v e} <?> "List Statement")

seqList::Parser [[SeqField]]
seqList= lexeme ( (squares $ sepBy singleSequence bar)<?> "Sequence")  

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
        x <- lexeme ((try(seqSingle) <|> try(seqDisj) <|> try(seqNeg) <|> try(seqStar))<?>" Sequence Event")
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
event =  lexeme ((try (eSome) <|> try (liftM EAll eventName))<?>"Event")
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
barchart::Parser Var
barchart =  lexeme (
    do{
        reserved "barchart";
        v <- var;
        semi;
        return $ v}<?> "Barchart")

foreach::Parser ForEachDef
foreach =lexeme(
    do{  f<-try(forEachFilter) <|> try(forEachTable) <|> try(forEachSequence) <|> try(forEachList);
        optional semi;
        return f;
} <?> "For Each Definition")

forEachFilter::Parser ForEachDef
forEachFilter =
    do
        reserved "foreach"
        f <- lexeme filterName
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
        reserved "member"
        v1 <- var
        reserved "in"
        v2 <- var
        return $ ForEachList v1 v2

printvar::Parser PrintAction
printvar =
    do
        reserved "print"
        v <- var
        semi
        return $ PrintVar v

prints:: Parser PrintAction
prints = lexeme ( 
    do{
   x<-(try printvar <|> try printTimeLine <|> try printLength <|> try printFilters <|> try printElement);
   
   return x;
   } <?> "Print Statement")


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
        v2 <- between (symbol "[" <?> "Table index \"[]\"") (symbol "]") var
        semi  
        return $ PrintElement v1 v2

filterName::Parser FilterName
filterName = lexeme $
    do
        f <- identifier
        return (map toLower f)

filterVal::Parser FieldVal
filterVal = lexeme( groupItem <?> "Field Options")
        

filters :: Parser Filter
filters = lexeme(
    do  {
        filtName <- lexeme (identifier <?> "Filter Section Name");
        choice $ [reserved "is", reserved "are"];
        semi;
        filterDs <- lexeme $ some $ try filterDefs;
        return $ Filter (map toLower filtName) filterDs} <?> "Filter Section")

manyFilters :: Parser [Filter]
manyFilters = lexeme (
    do {
        many filters
        } <?> "Filters Block")


filterDefs :: Parser FieldDef 
filterDefs = lexeme (
    do {
        ffield <- identifier;
        colon;
        fval <- sepBy filterVal comma;
        semi;
        return $ FieldDef ((map toLower ffield)) fval
        } <?> "Field Definition ")


useList :: Parser UseFile
useList = lexeme (
    do {
        reserved "use";
        names <- sepBy grpFile comma;
        semi;
        return $ UseFile names;}<?> "Use statement")


grpFile::Parser String
grpFile = lexeme (
    do {
        file <- some alphaNum;
        dot;
        string "grp";
        return file;} <?> "use file" )
