{-
OncoTime - Implementation of cancer-research domain-specific language as a project undertaken for
COMP 520 - Compiler Design in Winter 2016 at McGill University by

Shivan Kaul Sahib
Yusaira Khan
Brendan Games Gordon

The course was taught by Laurie Hendren.
 -}

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
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char(isAlphaNum)
import TypeUtils
import Data.Char
import Debug.Trace
import Types
import Text.Parsec.Pos
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

oncoParser:: Parser ((Program Annotation), SourcePos)
oncoParser =
    do
        whiteSpace
        hdr <- header
        doc <- documentation
        use <- many useList
        grp <- many groups
        filt <- manyFilters
        comp <- manyComp
        p <- getPosition
        return $ (Program hdr doc use grp filt comp,p)

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
        pos <- getPosition --some alphaNum
        return $ Var var (Annotation "") pos

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
        pos <- getPosition
        v <- var
        reserved "="
        grpItem <- curlies $ sepBy (formattedGroup (getGroupType grpType)) comma
        semi
        return $ Group grpType (Var (getVar v) (Annotation (getGroupType grpType)) pos) grpItem--(Annotation (getGroupType groupType))) grpItem


getGroupType:: GroupType -> String
getGroupType (GroupType a _) = a

getVar::Var a ->String
getVar (Var v _ _) = v


formattedGroup::(String)->Parser (GroupItem Annotation)
formattedGroup name =
    do
      (optional semi)
      z<-groupItem name
      (optional semi)
      return z

groupType::Parser (GroupType)
groupType = lexeme $
    do
        pos <- getPosition
        gt <- some alphaNum
        return $ GroupType (map toLower gt) pos


groupItem::(String)->Parser (GroupItem Annotation)
groupItem name = try  (groupValDate name )
        <|> try (groupRange name)
        <|> try (groupValString name)
        <|> try  (groupVar name)

groupVar ::String-> Parser (GroupItem Annotation)
groupVar name =
    do
        gv <- angles $ var
        pos <- getPosition
        return $ GroupVar (Var (getVar gv) (Annotation (map toLower name)) pos)

groupValString::String->Parser (GroupItem Annotation)
groupValString name = lexeme $
    do
        gv <- some wordChar
        return $ GroupValString (map toLower gv) (Annotation (map toLower name)) -- ???

groupValDate::String->Parser (GroupItem Annotation)
groupValDate name = lexeme (
    do {
        y <- some digit;
        date_sep;
        m <- some digit;
        date_sep;
        d <- some digit;
        return $ GroupDate (read y) (read m) (read d) (Annotation (map toLower name)) } <?> "Date")



groupRange::String->Parser (GroupItem Annotation)
groupRange name = try ( betw name  ) <|> try (before name)<|> try (after name)  <|> try (single name) <?> "Number or Range"

before::String->Parser (GroupItem Annotation)
before name =
    do
        reserved "before"
        pre <- some digit
        return $ GroupRange ((Before (read pre)) (Annotation (map toLower name)))

after::String->Parser (GroupItem Annotation)
after n =
    do
        reserved "after"
        post <- some digit
        return  $ GroupRange ( (After  (read post)) (Annotation (map toLower n)))

betw::String->Parser (GroupItem Annotation)
betw n = lexeme $
    do
        pre <- lexeme $ some digit
        reserved "to"
        post <- lexeme $ some digit
        return $ GroupRange ((Between (read pre) (read post)) (Annotation (map toLower n)))


single::String->Parser (GroupItem Annotation)
single n = lexeme $
    do
        gd <- some digit
        return $ GroupRange ( (SingleInt (read gd)) (Annotation (map toLower n)))


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
    try (liftM3 Foreach foreach manyComp getPosition{-(try-} {- <|> singleComp)-})
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
        x <-    try(seqStar)<|> try (seqComma) <|>
          try(seqNeg) <|>  try(seqBar)<?>"Sequence Event"
        (optional semi)
        return x


seqStar :: Parser (SeqField Annotation)
seqStar =
    do
        e <- curlies $ sepBy1 (do {(optional semi) ;
             e<-event; (optional semi);return e}) comma
        star
        return $ Star e
seqNeg::Parser (SeqField Annotation)
seqNeg =
    do
        e <- parens $ reserved "not" >> event --TODO: Write this better
        return $ Neg e

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
    do{  f<-try(forEachFilter) <|> try(forEachTable) <|>
        try(forEachSequence) <|> try(forEachList);
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
        p <- getPosition
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
   x<-(try printvar <|> try printTimeLine <|>
    try printLength <|> try printFilters <|> try printElement);

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

filterVal::String->Parser (FieldVal Annotation)
filterVal name = lexeme( groupItem name<?> "Field Options")


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
        fval <- sepBy1 (filterVal ffield) comma;
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
