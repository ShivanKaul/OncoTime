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

--the function that does all weeding
weed::String->Program->IO(Program)
weed file prg@(Program hdr docs useList groupDefs filters comps) =
    do
        --get Config file
        let conf = readConf file
       --grpFile weeding
        dirContents <- getDirectoryContents "."
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles
        let grpFileList = weedGroupFiles useList grpFileNames
        let useFilesToParse = map (\x -> "programs/valid/" ++ x ++ ".grp") (flattenUseFile useList)

        putStrLn ("Group files are " ++ (show useFilesToParse))

        case grpFileList of
            Left e -> putStrLn (file ++ ": ") >> print e >> exitFailure
            Right r -> putStrLn $ file ++ ": All Group files exist"

        --parsing each group file
        let grpAllFilesContents = map (readFile) (useFilesToParse)
        groups <- sequence (map (getGroupDefs) (grpAllFilesContents))
        let newGroups = concat (groups)

        --verify filters
        putStrLn "Weeded successfully"
        return (Program hdr docs [] (newGroups ++ groupDefs) filters comps)


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

{-
--should return files that are not elem of
--returns found files
compareUseLists::[String]->[String]->[String]
compareUseLists [] [] = []
compareUseLists grpFiles [] =  []
compareUseLists grpFiles (useFile:ys) =
    case useFile `elem` grpFiles of
        False -> useFile ++ (compareUseLists grpFiles ys)
        True -> (compareUseLists grpFiles ys)
compareUseLists [] useFile = []
-}

-- Group file names, parsing them individually, building up a list of
-- groupdefs which are then appended to parse output of parser


-- inlineGroupFiles :: [String] -> [GroupDefs]
-- inlineGroupFiles grpFiles =
--     do
--         let readFiles = map (readFile) grpFiles
--         groupDefs <- concat (map (getGroupDefs) (readFiles))
--         return groupDefs

getGroupDefs :: IO(String) -> IO([GroupDefs])
getGroupDefs grpFileData =
    do
        readData <- grpFileData
        case parse (manyGroups) "" (readData) of
            Left e -> putStrLn ("ERROR: " ++ show e) >> return []
            Right r -> return r

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
