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
        let conf = readConfig file

       --grpFile weeding
        dirContents <- getDirectoryContents "."
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles
        let grpFileList = weedGroupFiles useList grpFileNames
        let useFilesToParse = map (\x -> "programs/valid/" ++ x ++ ".grp")
                                  (flattenUseFile useList)

        putStrLn ("Group files are " ++ (show useFilesToParse))

        case grpFileList of
            Left e -> putStrLn (file ++ ": ") >> print e >> exitFailure
            Right r -> putStrLn $ file ++ ": All Group files exist"

        --parsing each group file
        let grpAllFilesContents = map (readFile) (useFilesToParse)
        groups <- sequence (map (getGroupDefs) (grpAllFilesContents))


        --check erroneous subfields i.e. whether all fields exist
        --let fil = checkFilters filters [] conf 
        
        --checking field redeclarations
        --checkFilterRedec filters [] conf


        --redeclarations of foreach

        --table syntax checking

        --verify filters
        putStrLn "Weeded successfully"
        return prg
        
        let newGroups = concat (groups)

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
                    False -> Left $ MissingFilesError
                        ("ERROR: Missing one of group files: " ++
                        ( intercalate ","  declaredUseFiles) ++
                        " out of: " ++ (intercalate "," grpFiles))
                    --Better error messages for other cases.
                    -- Maybe see what files are missing exactly.
                    -- Doesn't need to be true false exactly
                    True -> Right $ useList

getGroupDefs :: IO(String) -> IO([GroupDefs])
getGroupDefs grpFileData =
    do
        readData <- grpFileData
        case parse (manyGroups) "" (readData) of
            Left e -> putStrLn ("ERROR: " ++ show e) >> return []
            Right r -> return r

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
            False -> Left $ MissingFilesError "ERROR: Group files Missing"
            -- Better error messages for other cases.
            -- Maybe see what files are missing exactly.
            -- Doesn't need to be true false exactly
            True -> Right $ useFiles

--go through the list and make sure each filter is in the list, also that it isn't redeclared

{-
checkFilters::[Filter]->[Filter]->Config->Either LexError [Filter] 
checkFilters [] checkedList conf = checkedList
checkFilters (f:fs) [] conf  = checkFilters (f ++ []) conf
checkFilters ((Filter f def):fs) checkedList  conf = 
    case filter (== f) (map (\(Filter fn fd) -> fn)  checkedList) of 
        [] ->  checkFilters fs (checkedList ++ f) conf
        _ -> return $ FieldNameError "Error. The field " ++ f ++ "has been redeclared"

-}

{-
checkFilters::[Filters]->[Filters]->Config->Either LexError [Filters] 
checkFilters [] checkedList conf = 
checkFilters (f:fs) [] conf  = checkFilters (f ++ []) conf
checkFilters ((Filter f def):fs) ((Filter ch cdef):cs) conf = 
    case f `elem' checkedList of
        True = return $ FieldNameError "Error. The field " ++ f ++ "has already been declared"
        False = checkFilters fs (checkedList ++ f) conf

-}

