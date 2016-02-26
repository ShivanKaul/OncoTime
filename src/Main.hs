{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Directory
import System.FilePath
import System.Exit
import System.Environment
import System.IO
import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char

import qualified Text.ParserCombinators.Parsec.Token as Token

import TypeUtils
--Our modules
import Types 
import Parser
import PrettyPrinter
import Formatter

parseFile :: String->[Conf]->IO ()
parseFile file groupFileList =
    do 
        program <- readFile file
        case parse ((oncoParser  )<* eof) file (formatFile program) of
            Left e ->
                do
                    putStrLn "ERROR"
                    print e
            Right r -> print r

parseString :: String -> Program
parseString str =
    case parse (oncoParser <* eof) "" str of
        Left e-> error $ show e
        Right r -> r

--pass in a list of strings and watch it become something amazing!!!
--ConfMap
tparseFile :: String ->IO ()
tparseFile file =
    do 
        program <- readFile file
        case parse (testParser <* eof) file program of
            Left e ->
                do
                    putStrLn "ERROR"
                    print e
            Right r -> print r 

tparseString :: String -> TestProgram
tparseString str =
    case parse (testParser <* eof) "" str of
        Left e-> error$ show e
        Right r -> r
{-
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


    --if M.member sf mapping then True else subFieldExists xs sf
--subFieldTypeCheck::[Conf]->FieldName->SubFieldName->->Bool
{-
getSubFieldType::[Conf]->FieldName->SubFieldName->Either LexError AllowedType
getSubFieldType [] _ _ = Left $ AllowedType "ERROR. Element not found in Config"
getSubFieldType ((Conf (name, mapping)):xs) f sf = 
    case name == f of
        True ->  if M.member sf mapping then lookup M.member else Left $ AllowedType "ERROR. SubField not found"
        False -> getSubFieldType xs f sf

-}
--subFieldValCheck::Conf->SubField->Bool

testFieldStuff::IO()
testFieldStuff = 
    do
        readData <-readFile "config.conf"
        let l = lines readData
        let listOfMaps = map makeConf l
        print listOfMaps
        print $ fieldExists listOfMaps "Population" 
        print $ subFieldExists listOfMaps "Population" "Sex"
        print $ getConfWithField listOfMaps "Population"-}
main = 
    do
        readData <- readFile "config.conf"
        (args:_) <- getArgs 
        --args <- getArgs
        let l= lines readData
        let listOfMaps =  map makeConf l
        
        --mapM 
        parseFile args listOfMaps
        --parseFile grpFiles

        --let m =  M.fromList listOfMaps
    --print m 
    --  
        print listOfMaps
        --print $ M.fromList listOfMaps
        --print reconstructed 
        --parseFile arg
