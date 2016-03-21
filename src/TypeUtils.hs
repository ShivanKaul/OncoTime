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
        sf <-  try fieldtype  <|> try fieldval <?> "not a val list or a valid type"               --sf <- choice [ (squares (sepBy identifier comma)), (curlies (sepBy identifier comma)), identifier]

        
        return sf

fieldval::Parser Field
fieldval = lexeme $
    do
        p <-( squares  (sepBy (some alphaNum) comma) )
        return $ FieldVal p


fieldtype::Parser Field
fieldtype = lexeme $
    do
        p <- some alphaNum
    
        case p of
            "String" -> return (FieldType "String")
            "Int" -> return (FieldType "Int")
            _ -> case ((reads p)::[(Int, String)]) of
                        [(_,"")] -> return (FieldType "Int") 
                        _ -> return (FieldType "String")



configParser::Parser Config
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

configListToMap::[Config]->(Map (FieldName, Bool) FieldMap)
configListToMap ((Config (x)):[]) = 
    case M.toList x of
        [] -> M.empty
        ((fname,loop), sub):[] -> M.singleton (fname,loop) sub
        mapList -> x 

configListToMap ((Config x):xs) = M.union x $ configListToMap xs

configToMap::Config->(Map (FieldName, Bool) FieldMap)
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
        --print $ fieldExists (Config totalMap) "Population" "Sex"
        --print $ fieldExists (Config totalMap) "Population" "Flarf"
        --print $ getConfWithField listOfMaps "Population"
        print "fields tested" 
