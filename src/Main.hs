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
import Debug.Trace

--Our modules
import Types 
import Parser
import PrettyPrinter
import Formatter
import Weeder

parseFile :: String -> IO ()
parseFile file =
    do 
        -- Check if file ends with .onc
        if takeExtension file /= ".onc" 
            then die ("ERROR: while reading " ++ file ++ ": File extension not .onc")
            else do
                program <- readFile file
                case parse (getFileName) "" program of
                    Left e ->
                        do
                            hPutStrLn stderr ("ERROR for file: " ++ (takeBaseName file) ++ show e)
                    Right r -> if takeBaseName file /= r 
                        then do
                            die ("ERROR: while reading " ++ file ++ ": Filename does not match script name")
                        else do
                            -- case parse ((oncoParser  )<* eof) file (trace (formatFile program) (formatFile program) )of --debugging
                            case parse ((oncoParser  )<* eof) file (formatFile program) of
                                Left e ->
                                    do
                                        hPutStrLn stderr ("ERROR: " ++ show e)
                                Right r -> writeFile ((reverse (drop 4 (reverse file))) ++ ".pretty.onc") (pretty r) >> print "VALID"

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
                    hPutStrLn stderr ("ERROR: " ++ (show e))
            Right r -> print r 

tparseString :: String -> TestProgram
tparseString str =
    case parse (testParser <* eof) "" str of
        Left e-> error$ show e
        Right r -> r

tparseFileCheck :: String ->IO ()
tparseFileCheck file =
    do 
        readData <- readFile "config.conf"
        let l= lines readData
        let listOfMaps =  map makeConf l
        program <- readFile file
        case parse ((testParserCheck listOfMaps) <* eof) file program of
            Left e ->
                do
                    putStrLn "ERROR"
                    print e
            Right r -> print r 
main = 
    do
        --readData <- readFile "config.conf"
        (args:_) <- getArgs 
        --args <- getArgs
        -- let l = lines readData
        -- let listOfMaps =  map makeConf l
        parseAndWeed args
        --parseFile args
