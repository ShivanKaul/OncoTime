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
import Types
-- import PrettyPrinter
import Lexer


data Config =  Config (Map FieldName (Map SubFieldName (SubField))) deriving(Eq, Show)

data Conf = Conf (FieldName, (Map SubFieldName (SubField))) deriving(Eq, Show)
--type SubField = (AllowedType, [AllowedVal])
type SubField = (AllowedType, [AllowedVal])


--data TypesMap = TypesMap FieldName (M.Map AllowedType [AllowedVal])
type FieldName = String
type AllowedType = String
type AllowedVal = String
type SubFieldName = String

makeConfig:: String -> Config
makeConfig str =
    case parse (configParser <* eof) "" str of
        Left e-> error$ show e
        Right r -> r


--SubFieldName (AllowedType, AllowedVal)
typeMapMaker::Parser (String, (String, [String])) 
typeMapMaker =
    do
        name <- stringLit 
        colon
        tup <- parens $ typeTuple
        return (name, tup) 

subFieldMapMaker::Parser (Map SubFieldName (AllowedType, [AllowedVal]))
subFieldMapMaker = 
    do
        name <- stringLit 
        colon
        tup <- parens $ typeTuple 
        return (M.singleton name tup) 

typeTuple::Parser (AllowedType, [AllowedVal])
typeTuple =
    do
        alType <- identifier
        comma
        alVals <- squares $ sepBy identifier comma
        return (alType, alVals)

--data Config =  Config (M.Map FieldName (M.Map SubFieldName (SubField)))
configParser::Parser Config
configParser =
    do
        whiteSpace
        fieldName <- identifier
        colon
        --reserved "["
        typeMapList <- squares $ sepBy subFieldMapMaker comma
        --concat list of maps to a single one
        let subMapList = foldr M.union M.empty typeMapList 
        --reserved "]"
        semi
        --M.fromList typeMapList
        --return $ Config (M.singleton fieldName (M.fromList typeMapList)) 
        return $ Config (M.singleton fieldName subMapList)


getTypeFromMap::(SubField)->AllowedType
getTypeFromMap (a,b) = a

getValFromMap::(SubField)->[AllowedVal]
getValFromMap (a,b) = b


configListToMap::[Config]->(Map FieldName (Map SubFieldName (SubField)))
--configLisToMap ((Config (Map fn (Map sfn (sf)))):xs) = M.union 
configListToMap ((Config []):M.empty) = M.empty
configListToMap ((Config x):[]) = M.singleton  
configListToMap ((Config x):xs) = M.union x $ configListToMap xs
configListToMap ((Config []):xs) = configListToMap xs 

configToMap::Config->(Map FieldName (Map SubFieldName (SubField)))
configToMap (Config conf) = conf
--configLisToMap ((Config (Map fn (Map sfn (sf)))):xs) = M.union 

--data Config =  Config (Map FieldName (Map SubFieldName (SubField))) deriving(Eq, Show)

testFieldStuff::IO()
testFieldStuff = 
    do
        readData <-readFile "config.conf"
        let l = lines readData
        let listOfMaps = map makeConfig l
        
  --      let totalMap = foldr M.union M.empty listOfMaps

        --GET RID OF LIST OF MAPS. STUPID STUPID STUPID 
        let totalMap = configListToMap listOfMaps
        print $ totalMap
        --print $ M.toList totalMap
        --print $ fieldExists listOfMaps "Population" 
        --print $ subFieldExists listOfMaps "Population" "Sex"
        --print $ getConfWithField listOfMaps "Population"

