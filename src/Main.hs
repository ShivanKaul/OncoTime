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
makeConf:: String -> Conf 
makeConf str =
    case parse (confParser <* eof) "" str of
        Left e-> error$ show e
        Right r -> r

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

--util functions for checking parts of a conf
fieldExists::[Conf]->FieldName->Bool
fieldExists [] _ = False
fieldExists ((Conf (name, mapping)):xs) f = 
    if f == name then True else fieldExists xs f


subFieldExists::[Conf]->FieldName->SubFieldName->Bool
subFieldExists [] _ _ = False
subFieldExists ((Conf (name, mapping)):xs) f sf = 
    case name == f of
        True -> if M.member sf mapping then True else False
        False -> subFieldExists xs f sf

    --if M.member sf mapping then True else subFieldExists xs sf

--subFieldTypeCheck::[Conf]->FieldName->SubFieldName->->Bool


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
