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

--parse file that weeds
parseAndWeed::String->IO(Program)
parseAndWeed file =
    do
        {-
        program <- readFile file
        readData <- readFile "config.conf"
        let l= lines readData
        let listOfMaps =  map makeConf l
        dirContents <- getDirectoryContents "."
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles 
        -}
        -- Check if file ends with .onc
        if takeExtension file /= ".onc" 
            then do die ("ERROR: while reading " ++ file ++ ": File extension not .onc")
            else do
                program <- readFile file
                -- case parse ((oncoParser  )<* eof) file (trace (formatFile program) (formatFile program) )of --debugging
                case parse ((oncoParser)<* eof) file (formatFile program) of
                    Left e ->
                        do
                            putStrLn "ERROR" >> print e>> exitFailure
                            --print e
                    --Right r -> print r >> writeFile ((reverse (drop 4 (reverse file))) ++ ".pretty.onc") (pretty r)
                    --Right r-> weed grpFileNames listOfMaps r
                    Right parsedProg -> weed file parsedProg 

--the function that does all weeding
weed::String->Program->IO(Program)
weed file prg@(Program hdr docs useList groupDefs filter comps) =
    do
        let conf = readConf file
        let grpFileList = weedGroupFiles useList
        --let resu = weedProgram grpFileList conf prg 
        --verify grousp here
        --verify filters
        return prg
        --case resu of
          --  Left e -> print e >> exitFailure  --CATCH ALL ERRORS HERE
           -- Right r -> putStrLn "weeded successfully" >> return r
      
weedGroupFiles::[UseFile]->IO(Either LexError [UseFile])
weedGroupFiles useList = 
    do
        dirContents <- getDirectoryContents "."
        let grpFiles = map dropExtension $ filter (\x -> takeExtension x == ".grp") dirContents
        let declaredUseFiles = flattenUseFile useList
        case (null $ filter (not . (`elem` grpFiles)) declaredUseFiles) of
        --case (sort declaredUseFiles) == (sort grpFiles) of
            False -> return $ Left $ MissingFilesError "ERROR: Group files Missing" --Better error messages for other cases. Maybe see what files are missing exactly. Doesn't need to be true false exactly
            True -> return $ Right $ useList


readConf::String->IO([Conf])
readConf file = 
    do
        program <- readFile file
        readData <- readFile "config.conf"
        let l= lines readData
        let listOfMaps =  map makeConf l
        return listOfMaps

weedProgram::[Conf]->Program->Either LexError Program
weedProgram conf (Program hdr docs useList groupDefs filter comps) =
    do
        --test Group Files
        --test Conf
        --verifyFilters conf filter
        --let a = vFilters conf filter
        --pure $
        return (Program hdr docs useList groupDefs filter comps)

flattenUseFile::[UseFile]->[String]
flattenUseFile ((UseFile []):[]) = [] 
flattenUseFile ((UseFile x):[]) = x
flattenUseFile ((UseFile []):xs) = flattenUseFile(xs)
flattenUseFile ((UseFile x):xs) = x ++ flattenUseFile(xs)
flattenUseFile _ = []

testGroupFiles::[UseFile]->[String]->Either LexError [UseFile]
testGroupFiles useFiles grpFiles =
    do
        --Flatten the useFile List
        let declaredUseFiles = flattenUseFile useFiles
        case (sort declaredUseFiles) == (sort grpFiles) of
            False -> Left $ MissingFilesError "ERROR: Group files Missing" --Better error messages for other cases. Maybe see what files are missing exactly. Doesn't need to be true false exactly
            True -> Right $ useFiles

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
