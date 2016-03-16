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
--subFieldMapMaker::Parser (Map FieldName (AllowedType, [AllowedVal]))
subFieldMapMaker::Parser (Map FieldName Field)
subFieldMapMaker = lexeme $
    do
        name <- stringLit 
        colon
        subf <- parents $ subfield
        return (M.singleton (map toLower name) subf)
        --tup <- parens $ typeTuple 
        --return (M.singleton (map toLower name) tup) 

subfield::Parser Field 
subfield = --lexeme $
    do
        sf <-  try subfieldval <|>  try subfieldtype<|> try subfieldloopvals <?> "stupid"               --sf <- choice [ (squares (sepBy identifier comma)), (curlies (sepBy identifier comma)), identifier]

        
        return sf

subfieldval::Parser Field
subfieldval = lexeme $
    do
        p <-( squares  (sepBy identifier comma) )
        return $ FieldValue p

subfieldloopvals::Parser Field
subfieldloopvals = lexeme $
    do
        p <-( curlies  (sepBy identifier comma) )
        return $ FieldLoopVals p


subfieldtype::Parser Field
subfieldtype = lexeme $
    do
        p <- identifier
        return $ FieldType p 



configParser::Parser Config
configParser = lexeme $
    do
        whiteSpace
        fieldName <- identifier
        colon
        typeMapList <- squares $ sepBy subFieldMapMaker comma
        --concat list of maps to a single one
        --let subMapList = foldr M.union M.empty typeMapList 
        let subMapList = M.unions typeMapList
        semi
        return $ Config (M.singleton (map toLower fieldName) (FieldMap $  subMapList))

{-
getTypeFromMap::(Field)->AllowedType
getTypeFromMap (a,b) = a

getValFromMap::(Field)->[AllowedVal]
getValFromMap (a,b) = b
-}

--configListToMap::[Config]->(Map FieldName (Map FieldName (Field)))
configListToMap::[Config]->(Map FieldName FieldMap)
--configLisToMap ((Config (Map fn (Map sfn (sf)))):xs) = M.union 
configListToMap ((Config (x)):[]) = 
    case M.toList x of
        [] -> M.empty
        (fname, sub):[] -> M.singleton fname sub
        mapList -> x 
configListToMap ((Config x):xs) = M.union x $ configListToMap xs

configToMap::Config->(Map FieldName FieldMap)
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
