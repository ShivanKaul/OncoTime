module TypeUtils where

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
import Data.Char
import Debug.Trace
import Types
-- import PrettyPrinter
import Lexer


data Conf = Conf (FieldName, (M.Map SubFieldName (SubField))) deriving(Eq, Show)
type SubField = (AllowedType, [AllowedVal])

data TypesMap = TypesMap FieldName (M.Map AllowedType [AllowedVal])
type FieldName = String
type AllowedType = String
type AllowedVal = String
type SubFieldName = String

makeConf:: String -> Conf 
makeConf str =
    case parse (confParser <* eof) "" str of
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

typeTuple::Parser (AllowedType, [AllowedVal])
typeTuple =
    do
        alType <- identifier
        comma
        alVals <- squares $ sepBy identifier comma
        return (alType, alVals)

confParser::Parser Conf
confParser =
    do
        whiteSpace
        fieldName <- identifier
        colon
        --reserved "["
        typeMapList <- squares $ sepBy typeMapMaker comma
        --reserved "]"
        semi
        --M.fromList typeMapList
        return $ Conf (fieldName,  M.fromList typeMapList) --maps
        --Each FieldName is part of a tuple between  between SubField:(AllowedType, AllowedVals)


--util functions for checking parts of a conf
fieldExists::[Conf]->FieldName->Bool
fieldExists [] _ = False
fieldExists ((Conf (name, mapping)):xs) f = 
    if f == name then True else fieldExists xs f

getConfWithField::[Conf]->FieldName->Either LexError Conf 
getConfWithField [] _ = Left $ FieldNotFoundError "File Not in config list" 
getConfWithField ((Conf (name, mapping)):xs) f = 
    if f == name then Right $ (Conf (name, mapping)) else getConfWithField xs f

subFieldExists::[Conf]->FieldName->SubFieldName->Bool
subFieldExists [] _ _ = False
subFieldExists ((Conf (name, mapping)):xs) f sf = 
    case name == f of
        True -> if M.member sf mapping then True else False
        False -> subFieldExists xs f sf

getSubFieldVals::Conf->SubFieldName->[AllowedVal]
getSubFieldVals(Conf (name, mapping)) sf =  getValFromMap $ mapping M.! sf   

--shoudl get error maybe
getSubFieldType::Conf->SubFieldName->AllowedType
getSubFieldType (Conf (name, mapping)) sf =  getTypeFromMap $ mapping M.! sf
    

getTypeFromMap::(SubField)->AllowedType
getTypeFromMap (a,b) = a

getValFromMap::(SubField)->[AllowedVal]
getValFromMap (a,b) = b

testFieldStuff::IO()
testFieldStuff = 
    do
        readData <-readFile "config.conf"
        let l = lines readData
        let listOfMaps = map makeConf l
        print listOfMaps
        print $ fieldExists listOfMaps "Population" 
        print $ subFieldExists listOfMaps "Population" "Sex"
        print $ getConfWithField listOfMaps "Population"

