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

parseFile :: String->[Conf]->IO ()
parseFile file groupFileList =
    do 
        -- Check if file ends with .onc
        if takeExtension file /= ".onc" 
            then do die ("ERROR: while reading " ++ file ++ ": File extension not .onc")
            else do
                program <- readFile file
                -- case parse ((oncoParser  )<* eof) file (trace (formatFile program) (formatFile program) )of --debugging
                case parse ((oncoParser  )<* eof) file (formatFile program) of
                    Left e ->
                        do
                            putStrLn "ERROR"
                            print e
                    Right r -> print r >> writeFile ((reverse (drop 4 (reverse file))) ++ ".pretty.onc") (pretty r)

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

parseAndWeed::String->IO(Program)
parseAndWeed file =
    do
        program <- readFile file
        readData <- readFile "config.conf"
        let l= lines readData
        let listOfMaps =  map makeConf l
        -- Check if file ends with .onc
        if takeExtension file /= ".onc" 
            then do die ("ERROR: while reading " ++ file ++ ": File extension not .onc")
            else do
                program <- readFile file
                -- case parse ((oncoParser  )<* eof) file (trace (formatFile program) (formatFile program) )of --debugging
                case parse ((oncoParser  )<* eof) file (formatFile program) of
                    Left e ->
                        do
                            putStrLn "ERROR">> exitFailure
                            --print e
                    --Right r -> print r >> writeFile ((reverse (drop 4 (reverse file))) ++ ".pretty.onc") (pretty r)
                    Right r-> weed listOfMaps r

weed::[Conf]->Program->IO(Program)
weed conf prg@(Program hdr docs useList groupDefs filter comps) =
    do
        let resu = weedProgram conf prg 
        --verify grousp here
        --verify filters
        --
        case resu of
            Left e -> case e of --CATCH ALL ERRORS HERE
                (GenError e) -> putStrLn e >> exitFailure
            Right r -> putStrLn "weeded successfully" >> return r
       
weedProgram::[Conf]->Program->Either LexError Program
weedProgram conf (Program hdr docs useList groupDefs filter comps) =
    do
        --pure $
        return (Program hdr docs useList groupDefs filter comps)

main = 
    do
        readData <- readFile "config.conf"
        (args:_) <- getArgs 
        --args <- getArgs
        --let l= lines readData
        --let listOfMaps =  map makeConf l
        
        --let m =  M.fromList listOfMaps
    --print m 
    --
        parseAndWeed args

        print "SUCCESS"
        --print listOfMaps
        --print $ M.fromList listOfMaps
        --print reconstructed 
        --parseFile arg
