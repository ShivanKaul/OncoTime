{-
OncoTime - Implementation of cancer-research domain-specific language as a project undertaken for
COMP 520 - Compiler Design in Winter 2016 at McGill University by

Shivan Kaul Sahib
Yusaira Khan
Brendan Games Gordon

The course was taught by Laurie Hendren.
 -}

module TypeUtils where

import Types
import System.Exit
import System.Environment
import System.IO
import Data.Map (Map)
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
import Debug.Trace
import System.FilePath
import Types
-- import PrettyPrinter
import Lexer


makeConfig:: String -> (Config Annotation)
makeConfig str =
    case parse (configParser <* eof) "" str of
        Left e-> error $ show e
        Right r -> r


makeDBConfig::String->(DBConfig)
makeDBConfig str =
    case parse (dbConfigParser <* eof) "" str of
        Left e-> error $ show e
        Right r -> r

makeJoinConfig::String->(JoinConfig)
makeJoinConfig str =
    case parse (joinConfigParser <* eof) "" str of
        Left e-> error $ show e
        Right r -> r





{-

typeMapMaker::Parser (String, (String, [String]))
typeMapMaker =
    do
        name <- stringLit
        colon
        tup <- parens $ typeTuple
        return ((map toLower name), tup)

-}


--fieldMapMaker::Parser (Map FieldName (AllowedType, [AllowedVal]))
fieldMapMaker::Parser (Map FieldName (Field Annotation))
fieldMapMaker = lexeme $
    do
        name <- stringLit
        colon
        subf <- parents $ (fieldParse name)
        return (M.singleton (map toLower name) subf)
        --tup <- parens $ typeTuple
        --return (M.singleton (map toLower name) tup)


fieldParse::String->Parser (Field  Annotation)
fieldParse name = lexeme $
    do
        sf <-  try (fieldtype name) <|> try (fieldval name) <?> "not a val list or a valid type"               --sf <- choice [ (squares (sepBy identifier comma)), (curlies (sepBy identifier comma)), identifier]


        return sf
validChar :: Parser Char
validChar  = satisfy (\c -> (isAlphaNum c) || (c=='_'))

fieldval::String->Parser (Field Annotation)
fieldval name = lexeme $
    do
        --p <-( squares  (sepBy (some (choice[alphaNum, (oneOf "-_+.")] ) ) comma) )
        p <-( squares  (sepBy (some (validChar)) comma) )
        return $ FieldValue  (map (map toLower) p) (Annotation (map toLower name))

--validChar = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['!','-','_','.','+']

fieldtype::String->Parser (Field Annotation)
fieldtype name = lexeme $
    do
        p <- (some (validChar))

        case p of
            "String" -> return (FieldType "String" (Annotation (map toLower name)))
            "Int" -> return (FieldType "Int" (Annotation (map toLower name)))
            --_ -> case ((reads p)::[(Int, String)]) of
              --          [(_,"")] -> return (FieldType "Int")
                --        _ -> return (FieldType "String")

            _ -> return (FieldType "UNKNOWN" (Annotation (map toLower name)))

configParser::Parser (Config Annotation)
configParser = lexeme $
    do
        whiteSpace
        fieldName <- identifier
        colon
        --typeMapList <- squares $ sepBy fieldMapMaker comma
        b <- optionMaybe (oneOf "{")
        let c = case b of
                Just a -> True
                Nothing -> False

        typeMapList <- case c of
            True -> do
                p <- sepBy fieldMapMaker comma
                char '}'
                return p
            False ->  (squares $ sepBy fieldMapMaker comma)

       -- let typeMapList = reg

        --concat list of maps to a single one
        --let subMapList = foldr M.union M.empty typeMapList
        let fieldMapList = M.unions typeMapList
        semi
        return $ Config (M.singleton ((map toLower fieldName), (c)) (FieldMap $  fieldMapList))

dbConfigParser:: Parser (DBConfig)
dbConfigParser = lexeme $
    do
        whiteSpace
        cf <- identifier
        colon
        db <- stringLit

        semi
        return $ DBConfig (M.singleton cf db)

joinConfigParser:: Parser (JoinConfig)
joinConfigParser = lexeme $
    do
        whiteSpace
        reserved "ToJoinOn"
        colon
        joinableElement <- some alphaNum
        semi
        whiteSpace
        reserved "JoinableFields"
        colon
        joinableList <- parents $ sepBy (some alphaNum) comma
        semi
        return $ JoinConfig joinableElement joinableList

configListToMap::[(Config Annotation)]->(Map (FieldName, Bool) (FieldMap Annotation))
configListToMap ((Config (x)):[]) =
    case M.toList x of
        [] -> M.empty
        ((fname,loop), sub):[] -> M.singleton (fname,loop) sub
        mapList -> x
configListToMap ((Config x):xs) = M.union x $ configListToMap xs


dbConfigListToMap::[(DBConfig)]->(Map ConfigName DBField)
dbConfigListToMap ((DBConfig (x)):[]) =
    case M.toList x of
        [] -> M.empty
        (cfname ,dbname):[] -> M.singleton cfname dbname
        --mapList -> x
dbConfigListToMap ((DBConfig x):xs) = M.union x $ dbConfigListToMap xs





configToMap::(Config Annotation)->(Map (FieldName, Bool) (FieldMap Annotation))
configToMap (Config conf) = conf

--data Config =  Config (Map FieldName (Map SubFieldName (SubField))) deriving(Eq, Show)

---fieldExists::Config->FieldName->Bool
--fieldExists (Config confmap) fname = M.member fname confmap

--subFieldExists::Config->FieldName->SubFieldName->Bool
--subFieldExists (Config confmap) fname sfname =
 --   case trace (show (M.lookup fname confmap)) (M.lookup fname confmap) of
--        Nothing -> False
--        Just (SubMap m) -> M.member sfname m


testFieldStuff::IO()
testFieldStuff =
    do
        path <- getExecutablePath
        readData <- readFile $ (dropFileName path) ++"config.conf"

        let l = lines readData
        let listOfMaps = map makeConfig l
  --      let totalMap = foldr M.union M.empty listOfMaps
        --GET RID OF LIST OF MAPS. STUPID STUPID STUPID
        let totalMap = configListToMap listOfMaps
        --print $ M.toList totalMap
        --print $ fieldExists (Config totalMap) "Population"
        --print $ fieldExists (Config totalMap) "population"
        --print $ fieldExists (Config totalMap) "Population" "Sex"
        --print $ fieldExists (Config totalMap) "Population" "Flarf"
        --print $ getConfWithField listOfMaps "Population"
        print "fields tested"
