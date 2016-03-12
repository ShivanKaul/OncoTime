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
                    Right parsedProg -> print "Parsed. Now weeding." >> weed file parsedProg 

--the function that does all weeding
weed::String->Program->IO(Program)
weed file prg@(Program hdr docs useList groupDefs filters comps) =
    do
        --get Config file
        conf <- readConfig file 
        --print $ M.showTree $ configToMap conf
       --grpFile weeding
        dirContents <- getDirectoryContents "."
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles 
        let grpFileList = weedGroupFiles useList grpFileNames
        
        case grpFileList of 
            Left e -> putStrLn (file ++ ": ") >> print e >> exitFailure
            Right r -> putStrLn $ file ++ ": All Group files exist"
        --parsing each group file
       
        --verify filters
        putStrLn "Weeded successfully"
        
        return prg
        --case resu of
          --  Left e -> print e >> exitFailure  --CATALL ERRORS HERE
           -- Right r -> putStrLn "weeded successfully" >> return r

weedGroupFiles::[UseFile]->[String]->Either LexError [UseFile]
weedGroupFiles useList grpFiles = 
    do
        let declaredUseFiles = flattenUseFile useList

        if declaredUseFiles == [] 
            then Right $ useList 
            else 
                case (null $ filter (not . (`elem` grpFiles)) declaredUseFiles) of
        --case (sort declaredUseFiles) == (sort grpFiles) of
                    False -> Left $ MissingFilesError ("ERROR: Missing one of group files: " ++ ( intercalate ","  declaredUseFiles) ++ " out of: " ++ (intercalate "," grpFiles)) --Better error messages for other cases. Maybe see what files are missing exactly. Doesn't need to be true false exactly
                    True -> Right $ useList

readConfig::String->IO(Config)
readConfig file = 
    do
        program <- readFile file
        readData <- readFile "config.conf"
        let l= lines readData
        let totalMap = configListToMap $ map makeConfig l
        --print $ M.showTree $ totalMap 
        return $ Config totalMap


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
