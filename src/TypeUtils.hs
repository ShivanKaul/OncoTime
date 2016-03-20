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
--import Debug.Trace
import Types
-- import PrettyPrinter
import Lexer


makeConfig:: String -> Config
makeConfig str =
    case parse (configParser <* eof) "" str of
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
fieldMapMaker::Parser (Map FieldName Field)
fieldMapMaker = lexeme $
    do
        name <- stringLit 
        colon
        subf <- parents $ fieldParse
        return (M.singleton (map toLower name) subf)
        --tup <- parens $ typeTuple 
        --return (M.singleton (map toLower name) tup) 

fieldParse::Parser Field 
fieldParse = --lexeme $
    do
        sf <-  try fieldval <|>  try fieldtype <?> "stupid haha"               --sf <- choice [ (squares (sepBy identifier comma)), (curlies (sepBy identifier comma)), identifier]

        
        return sf

fieldval::Parser Field
fieldval = lexeme $
    do
        p <-( squares  (sepBy identifier comma) )
        return $ FieldVal p


fieldtype::Parser Field
fieldtype = lexeme $
    do
        p <- identifier
        return $ FieldType p 



configParser::Parser Config
configParser = lexeme $
    do
        whiteSpace
        fieldName <- identifier
        colon
        typeMapList <- squares $ sepBy fieldMapMaker comma
        --concat list of maps to a single one
        --let subMapList = foldr M.union M.empty typeMapList 
        let fieldMapList = M.unions typeMapList
        semi
        return $ Config (M.singleton (map toLower fieldName) (FieldMap $  fieldMapList))

configListToMap::[Config]->(Map FieldName FieldMap)
configListToMap ((Config (x)):[]) = 
    case M.toList x of
        [] -> M.empty
        (fname, sub):[] -> M.singleton fname sub
        mapList -> x 

configListToMap ((Config x):xs) = M.union x $ configListToMap xs

configToMap::Config->(Map FieldName FieldMap)
configToMap (Config conf) = conf

testFieldStuff::IO()
testFieldStuff = 
    do
        readData <-readFile "config.conf"
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
