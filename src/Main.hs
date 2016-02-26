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

parseFile :: String ->IO ()
parseFile file =
    do 
        program <- readFile file
        case parse (oncoParser <* eof) file (formatFile program) of
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

tparseFile :: String -> IO ()
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

main = do
    --(arg:_) <- getArgs
    dirContents <- getDirectoryContents "." 
    let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
    args <- getArgs
    mapM parseFile args
