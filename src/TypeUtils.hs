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


data Config =  Config (Map FieldName SubMap) deriving(Eq, Show)

data SubMap = SubMap (Map SubFieldName (SubField)) deriving(Eq, Show)

data Conf = Conf (FieldName, (Map SubFieldName (SubField))) deriving(Eq, Show)
type SubField = (AllowedType, [AllowedVal])

type FieldName = String
type AllowedType = String
type AllowedVal = String
type SubFieldName = String

makeConfig:: String -> Config
makeConfig str =
    case parse (configParser <* eof) "" str of
        Left e-> error$ show e
        Right r -> r

typeMapMaker::Parser (String, (String, [String])) 
typeMapMaker =
    do
        name <- stringLit 
        colon
        tup <- parens $ typeTuple
        return ((map toLower name), tup) 

subFieldMapMaker::Parser (Map SubFieldName (AllowedType, [AllowedVal]))
subFieldMapMaker = 
    do
        name <- stringLit 
        colon
        tup <- parens $ typeTuple 
        return (M.singleton (map toLower name) tup) 

typeTuple::Parser (AllowedType, [AllowedVal])
typeTuple =
    do
        alType <- identifier
        comma
        alVals <- squares $ sepBy identifier comma
        return (map toLower alType, alVals)

configParser::Parser Config
configParser =
    do
        whiteSpace
        fieldName <- identifier
        colon
        typeMapList <- squares $ sepBy subFieldMapMaker comma
        --concat list of maps to a single one
        let subMapList = foldr M.union M.empty typeMapList 
        semi
        return $ Config (M.singleton (map toLower fieldName) (SubMap subMapList))


getTypeFromMap::(SubField)->AllowedType
getTypeFromMap (a,b) = a

getValFromMap::(SubField)->[AllowedVal]
getValFromMap (a,b) = b


--configListToMap::[Config]->(Map FieldName (Map SubFieldName (SubField)))
configListToMap::[Config]->(Map FieldName SubMap)
--configLisToMap ((Config (Map fn (Map sfn (sf)))):xs) = M.union 
configListToMap ((Config (x)):[]) = 
    case M.toList x of
        [] -> M.empty
        (fname, sub):[] -> M.singleton fname sub
        mapList -> x 
configListToMap ((Config x):xs) = M.union x $ configListToMap xs

configToMap::Config->(Map FieldName SubMap)
configToMap (Config conf) = conf
--data Config =  Config (Map FieldName (Map SubFieldName (SubField))) deriving(Eq, Show)

fieldExists::Config->FieldName->Bool
fieldExists (Config confmap) fname = M.member fname confmap 

subFieldExists::Config->FieldName->SubFieldName->Bool
subFieldExists (Config confmap) fname sfname =  
    case (M.lookup fname confmap) of
        Nothing -> False
        Just (SubMap m) -> M.member sfname m

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
        --print $ subFieldExists (Config totalMap) "Population" "Sex"
        --print $ subFieldExists (Config totalMap) "Population" "Flarf"
        --print $ getConfWithField listOfMaps "Population"
        print "fields tested" 
