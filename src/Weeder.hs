{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Weeder where

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



parseAndWeed::String->IO(Program)
parseAndWeed file =
    do
        program <- readFile file
        readData <- readFile "config.conf"
        let l= lines readData
        let listOfMaps =  map makeConf l
        dirContents <- getDirectoryContents "."
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles 
        -- Check if file ends with .onc
        if takeExtension file /= ".onc" 
            then do die ("ERROR: while reading " ++ file ++ ": File extension not .onc")
            else do
                program <- readFile file
                -- case parse ((oncoParser  )<* eof) file (trace (formatFile program) (formatFile program) )of --debugging
                case parse ((oncoParser  )<* eof) file (formatFile program) of
                    Left e ->
                        do
                            putStrLn "ERROR WITH PARSER">> exitFailure
                            --print e
                    --Right r -> print r >> writeFile ((reverse (drop 4 (reverse file))) ++ ".pretty.onc") (pretty r)
                    Right r-> weed grpFileNames listOfMaps r

weed::[String]->[Conf]->Program->IO(Program)
weed grpFileList conf prg@(Program hdr docs useList groupDefs filter comps) =
    do
        let resu = weedProgram grpFileList conf prg 
        --verify grousp here
        --verify filters
        case resu of
            Left e -> case e of --CATCH ALL ERRORS HERE
                (GenError e) -> putStrLn e >> exitFailure
            Right r -> putStrLn "weeded successfully" >> return r
       
weedProgram::[String]->[Conf]->Program->Either LexError Program
weedProgram grpFiles conf (Program hdr docs useList groupDefs filter comps) =
    do
        testGroupFiles useList grpFiles
        --test Group Files
        --test Conf
        --verifyFilters conf filter
        --let a = vFilters conf filter

        --pure $
        return (Program hdr docs useList groupDefs filter comps)

testGroupFiles::[UseFile]->[String]->Either LexError [UseFile]
testGroupFiles g@((UseFile x):xs) grpFiles =
--testGroupFiles g@[(UseFile x)] grpFiles = 
    do
        --useList to group names 
        let a = verifyGroupFiles x 
        return g
        

--checkAll
--one for names
--one for fields 
--one for valls

checkSubFields::[Conf]->FilterName->[FilterDef]->Bool
checkSubFields [] fname [] = False
checkSubFields c fname [] = True
checkSubFields c fname ((FilterDef (FilterField fieldName) _):ys) =
    case (subFieldExists c fname fieldName) of
        True -> (checkSubFields c fname ys)
        False -> False

vFilters::[Conf]->[Filter]->Bool
vFilters conf [] = True
vFilters conf ((Filter fname a@((FilterDef (FilterField fieldName) _ ):ys)):xs) = 
    case (fieldExists conf fname) && (checkSubFields conf fname a) of
        True -> (vFilters conf xs) 
        False -> False
