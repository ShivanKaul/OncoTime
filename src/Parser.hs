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
testParser :: Parser (TestProgram Annotation)
testParser =
    do
        whiteSpace
        --try testHeader <|>testUse <|> testGroups <|> try testComputation <|>  try testDocs
        --testHeader
        --testUse
        -- testDocs
        testGroups
        --testFilters
        -- testComputation
--
--
--Check tester
--
--

--testProgram::Parser TestProgram -}
testParserCheck ::(Config Annotation)-> Parser (TestProgram Annotation)
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

testFiltersCheck::(Config Annotation)->Parser (TestProgram Annotation)
testFiltersCheck c=
    do
        --filters <- many (filtersCheck c)
        filters <- many filters
        return $ TestFiltersList filters

testHeader::Parser (TestProgram Annotation)
testHeader =
    do
        hdr <- header
        return $ TestHeader hdr

testDocs::Parser (TestProgram ())
testDocs =
    do
        docs <- documentation
        return $TestDocs docs

testUse::Parser (TestProgram ())
testUse =
    do
        use <- many useList
        return $ TestUseFileList use

testGroups::Parser (TestProgram Annotation)
testGroups =
    do
        grp <- many groups
        return $ TestGroupList grp 

testFilters::Parser (TestProgram Annotation) 
testFilters =
    do
        filt <- many filters
        return $ TestFiltersList filt

testComputation::Parser (TestProgram Annotation)
testComputation =
    do
        comp <- many computation
        return $ TestComputation comp

oncoParser:: Parser (Program Annotation)
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
header:: Parser (Header Annotation) 
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
{-
arg :: Parser Arg
arg =
    do
        t <- groupType
        v <- var
        return $ Arg t v
-}

arg :: Parser (Arg Annotation)
arg =
    do
        t <- groupType
        v <- var
        return $ Arg t v


--just gets the next string
var:: Parser (Var Annotation)
var = lexeme $
    do
        var <- identifier--some alphaNum
        return $ Var var (Annotation "")

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
        --(many anyChar) 
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

manyGroups :: Parser [(GroupDefs Annotation)]
manyGroups =
    do
        grps <- many groups
        return grps

groups::Parser (GroupDefs Annotation)
groups = lexeme $
    do
        reserved "group"
        grpType <- groupType
        v <- var
        reserved "="
        grpItem <- curlies $ sepBy formattedGroup comma
        semi
        return $ Group grpType (Var (getVar v) (Annotation (getGroupType grpType))) grpItem--(Annotation (getGroupType groupType))) grpItem 


getGroupType::GroupType ->String
getGroupType (GroupType a) =a

getVar::Var a->String
getVar (Var v _) = v


formattedGroup::Parser (GroupItem Annotation)
formattedGroup =
    do
      (optional semi)
      z<-groupItem
      (optional semi)
      return z

groupType::Parser (GroupType)
groupType = lexeme $
    do
        gt <- some alphaNum
        return $ GroupType (map toLower gt)

groupItem::Parser (GroupItem Annotation)
groupItem = try groupValDate
        <|> try groupRange
        <|> try groupValString
        <|> try groupVar

groupVar :: Parser (GroupItem Annotation)
groupVar =
    do
        gv <- angles $ var
        return $ GroupVar (Var (getVar gv) (Annotation ""))

groupValString::Parser (GroupItem Annotation)
groupValString = lexeme $
    do
        gv <- some wordChar
        return $ GroupValString gv (Annotation "") -- ???

groupValDate::Parser (GroupItem Annotation)
groupValDate = lexeme (
    do {
        y <- some digit;
        date_sep;
        m <- some digit;
        date_sep;
        d <- some digit;
        return $ GroupDate (read y) (read m) (read d)} <?> "Date")

groupRange::Parser (GroupItem Annotation)
groupRange = try ( (liftM GroupRange betw ) ) <|> try (liftM GroupRange before ) <|> try (liftM GroupRange after)  <|> try (liftM GroupRange single  ) <?> "Number or Range"

before::Parser (RangeType Annotation)
before =
    do
        reserved "before"
        pre <- some digit
        return $ Before (read pre) (Annotation "Int")

after::Parser (RangeType Annotation)
after =
    do
        reserved "after"
        post <- some digit
        return $ After  (read post) (Annotation "Int")

betw::Parser (RangeType Annotation)
betw = lexeme $
    do
        pre <- lexeme $ some digit
        reserved "to"
        post <- lexeme $ some digit     
        return $ Between (read pre) (read post) (Annotation "Int")


single::Parser (RangeType Annotation)
single = lexeme $
    do
        gd <- some digit
        return $ SingleInt (read gd) (Annotation "Int")


manyComp ::Parser [(Computation Annotation)]
manyComp = lexeme(
    do  {
        c <- between (symbol "{" <?> "Start of Computation Block \"{\"")
        (symbol "}" <?> "End of computation block \"}\"") $
        (optional semi) >> (many computation);
        if (null c)
            then trace ("WARNING: Computation list is empty.") (optional semi)
            -- then print ("WARNING: Computation list is empty.")
        else (optional semi);
        return c}<?> "Computation Block \"{}\"")

singleComp :: Parser [(Computation Annotation)] 
singleComp = lexeme( 
    do { 
    c <- computation;
    return [c] } <?> "Single Line Computation"
    )

computation::Parser (Computation Annotation)
computation =
    try (liftM2 Foreach foreach  manyComp {-(try-} {- <|> singleComp)-})
    <|> try (table)
    <|> try (list)
    <|> try (liftM Print prints)
    <|> try (liftM Barchart barchart)

table::Parser (Computation Annotation)
table = lexeme (
    do {
        reserved "table";
        v <- var;
        reserved "=";
        reserved "count";
        fn <- filterName;
        reserved "by";
        ffield <- identifier;
        semi;
        return $ Table v fn (map toLower ffield) }<?>"Table Statement") 

list::Parser (Computation Annotation)
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

seqList::Parser [(SeqField Annotation)] 
seqList= lexeme ( (squares $  formattedSequence )<?> "Sequence")  

formattedSequence::Parser [(SeqField Annotation)]
formattedSequence =
    do
        (optional semi)
        x <- sepBy seqField arrow
        (optional semi)
        return x

seqField::Parser (SeqField Annotation)
seqField =
    do
        (optional semi)
        x <-    try(seqStar)<|> try (seqComma) <|>  try(seqNeg) <|>  try(seqBar)<?>"Sequence Event"
        (optional semi)
        return x

 -- try(seqSingle) <|>
-- seqSingle::Parser SeqField
-- seqSingle = lexeme (
--     do{
--         e <- event;
--         return $ Single e}<?>"Single Event")
seqStar :: Parser (SeqField Annotation)
seqStar =
    do
        e <- curlies $ sepBy1 (do {(optional semi) ; e<-event; (optional semi);return e}) comma
        star
        return $ Star e
seqNeg::Parser (SeqField Annotation)
seqNeg =
    do
        e <- parens $ reserved "not" >> event --TODO: Write this better
        return $ Neg e
-- seqNot::Parser Event
-- seqNot =
--     do
--         reserved "not"
--         event

seqComma :: Parser (SeqField Annotation)
seqComma =
    do

        e <- curlies $ sepBy1 (do {(optional semi) ; e<-event; (optional semi);return e}) comma
        return $ Comma e
        
seqBar :: Parser (SeqField Annotation)
seqBar =
    do
        e <- sepBy1 (do {(optional semi) ; e<-event; (optional semi);return e}) bar
        return $ Bar e

event::Parser (Event Annotation)
event = lexeme $ 
    do--lexeme ((return $ Event  eventName (Annotation ""))  <?>"Event")

        e <- eventName
        return (Event e (Annotation ""))

eventName :: Parser (EventName)
eventName = identifier

barchart::Parser (Var Annotation)
barchart =  lexeme (
    do{
        reserved "barchart";
        v <- var;
        semi;
        return $ v}<?> "Barchart")

foreach::Parser (ForEachDef Annotation)
foreach =lexeme(
    do{  f<-try(forEachFilter) <|> try(forEachTable) <|> try(forEachSequence) <|> try(forEachList);
        optional semi;
        return f;
} <?> "For Each Definition")

forEachFilter::Parser (ForEachDef Annotation)
forEachFilter =
    do
        reserved "foreach"
        f <- lexeme filterName
        v <- var
        return $ ForEachFilter f v


forEachTable::Parser (ForEachDef Annotation)
forEachTable =
    do
        reserved "foreach"
        reserved "element"
        v1 <- var
        reserved "of"
        v2 <- var
        return $ ForEachTable v1 v2

forEachSequence::Parser (ForEachDef Annotation)
forEachSequence =
    do
        reserved "foreach"
        reserved "sequence"
        v1 <- var
        reserved "like"
        e <- seqList
        return $ ForEachSequence v1 e 

forEachList::Parser (ForEachDef Annotation)
forEachList =
    do
        reserved "foreach"
        reserved "member"
        v1 <- var
        reserved "in"
        v2 <- var
        return $ ForEachList v1 v2

printvar::Parser (PrintAction Annotation)
printvar =
    do
        reserved "print"
        v <- var
        semi
        return $ PrintVar v

prints:: Parser (PrintAction Annotation) 
prints = lexeme ( 
    do{
   x<-(try printvar <|> try printTimeLine <|> try printLength <|> try printFilters <|> try printElement);

   return x;
   } <?> "Print Statement")


printTimeLine::Parser (PrintAction Annotation) 
printTimeLine =
    do
        reserved "print"
        reserved "timeline"
        reserved "of"
        v<-var
        semi
        return $ PrintTimeLine v

printLength::Parser (PrintAction Annotation)
printLength =
    do
        reserved "print"
        v<-var
        dot
        reserved "length"
        semi
        return $ PrintLength v


printFilters::Parser (PrintAction Annotation)
printFilters =
    do
        reserved "print"
        filterList <- sepBy1 filterName comma
        reserved "of"
        v <- var
        semi
        return $ PrintFilters filterList v

printElement::Parser (PrintAction Annotation)
printElement =
    do
        reserved "print"
        v1 <-var
        v2 <- between (symbol "[" <?> "Table index \"[]\"") (symbol "]") var
        semi
        return $ PrintElement v1 v2

filterName::Parser (FilterName)
filterName = lexeme $
    do
        f <- identifier
        return (map toLower f)

filterVal::Parser (FieldVal Annotation)
filterVal = lexeme( groupItem <?> "Field Options")


filters :: Parser (Filter Annotation)
filters = lexeme(
    do  {
        filtName <- lexeme (identifier <?> "Filter Section Name");
        choice $ [reserved "is", reserved "are"];
        semi;
        filterDs <- lexeme $ some $ try filterDefs;
        return $ Filter (map toLower filtName) filterDs} <?> "Filter Section")

manyFilters :: Parser [(Filter Annotation)]
manyFilters = lexeme (
    do {
        many filters
        } <?> "Filters Block")


filterDefs :: Parser (FieldDef Annotation)
filterDefs = lexeme (
    do {
        ffield <- identifier;
        colon;
        fval <- sepBy1 filterVal comma;
        semi;
        return $ FieldDef ((map toLower ffield)) fval
        } <?> "Field Definition ")


useList :: Parser UseFile
useList = lexeme (
    do {
        reserved "use";
        names <- sepBy1 grpFile comma;
        semi;
        return $ UseFile names;}<?> "Use statement")


grpFile::Parser String
grpFile = lexeme (
    do {
        file <- some alphaNum;
        dot;
        string "grp";
        return file;} <?> "use file" )
