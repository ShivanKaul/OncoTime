{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Weeder where

import System.Directory
import System.FilePath
import System.Exit
import System.Environment
import System.IO
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as M 
import Data.Hashable
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
        conf <- readConfig file
       --grpFile weeding
        curContents <- (getDirectoryContents ".") 
        valContents <-  (getDirectoryContents "./programs/valid/" ) 
        
        --dirContents <-  (getDirectoryContents "./programs/valid/" ) 
        invContents <- (getDirectoryContents "./programs/invalid/")
        let dirContents = curContents ++ valContents ++ invContents
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles
        let grpFileList = weedGroupFiles useList grpFileNames
        let useFilesToParse = map (\x -> "programs/valid/" ++ x ++ ".grp") (flattenUseFile useList)

        --putStrLn ("Group files are " ++ (show useFilesToParse))
        case grpFileList of
            Left e -> putStrLn (file ++ ": ") >> print e >> exitFailure
            Right r -> putStrLn $ file ++ ": All Group files exist"

        --parsing each group file
        --let grpAllFilesContents = map (readFile) (useFilesToParse)
        --newGroups <- sequence (map (getGroupDefs) (grpAllFilesContents))

        --check erroneous subfields i.e. whether all fields exist
       
        case (checkFilters filters conf) of
            Left e -> print e >>  putStrLn "FILTERS:" >> print filters >> putStrLn "CONF:" >> print conf >>  exitFailure 
            Right r -> putStrLn "All Fields valid"

        --checking field redeclarations
        --checkFilterRedec filters [] conf

        --redeclarations of foreach

        --table syntax checking

        --verify filters
        --let allGroups = (concat (newGroups)) ++ groupDefs
        let allGroups = groupDefs

        -- SAMPLE USES OF SYMBOL TABLE
        -- let symbolTable1 = buildSymbolTable allGroups hdr
        -- testIfSymbolTableContains symbolTable1 (Var "x")

        putStrLn "Weeded successfully"
        return (Program hdr docs [] (allGroups) filters comps)

-- Make Var hashable
instance (Hashable Var) where
  hashWithSalt s (Var v) = s + (hash v)

-- Utility test function to check if symbol table contains a key
testIfSymbolTableContains :: HashMap.HashMap Var GroupType -> Var -> IO()
testIfSymbolTableContains hashmap (Var v) =
        case (HashMap.lookup (Var v) hashmap) of
            Nothing -> print "nothing found!"
            Just r -> print ("Found VALUE " ++ show r ++ " for KEY " ++
                v ++ " in symboltable1")

-- Build symbol table from groups
buildSymbolTable :: [GroupDefs] -> Header -> HashMap.HashMap Var GroupType
buildSymbolTable groups (Header _ args) =
    do
        let keyValuesGroups = map (\(Group (t) (v) _) -> (v, t)) (groups)
        let keyValuesHeader = map (\(Arg (t) (v)) -> (v, t)) (args)
        (HashMap.fromList (keyValuesGroups ++ keyValuesHeader))

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
            False -> Left $ MissingFilesError "ERROR: Group files Missing" --Better error messages for other cases. Maybe see what files are missing exactly. Doesn't need to be true false exactly
            True -> Right $ useFiles

checkFilters::[Filter]->Config->Either LexError [Filter] 
checkFilters filList conf = case (checkFilRedec filList ) of
    Right r -> 
        case (checkSubFieldsEx conf r [] ) of
            [] -> Right filList
            l -> Left $ MissingConfigField $ "Error. Subfields Missing in " ++ (M.showTreeWith (\k x -> show (k,x)) True False (M.fromList l) )
    Left e -> Left e 
--Highest level, checkFilters. Is in the either monad to give us error checking

getFilterName::Filter->FilterName
getFilterName (Filter f _) = f

checkFilRedec::[Filter]->Either LexError [Filter]
checkFilRedec [] = Right []
checkFilRedec x =
    case (getRedeclarations x []) of
        [] -> Right $ x
        y -> Left $ RedecError ("The following filters were redeclared: " ++ (intercalate ", " (map (getFilterName) y)) )
--checkFilRedec a@_ = Left $ MissingConfigFile ("ERROR: Missing or Empty Config File Specified " ++ "\n Filters:" ++ (intercalate ", " (map (getFilterName) a )))
 --  ++ (intercalate ", " (map (getFilterName) redList))

--creates a list of all repeated elements of a list
getRedeclarations::(Eq a)=>[a]->[a]->[a]
getRedeclarations [] [] = []
getRedeclarations [] checkedList = checkedList
getRedeclarations (x:[]) [] = []
getRedeclarations (x:xs) checkedList =
    case x `elem` xs of
        True -> getRedeclarations ((filter (/= x)) xs) (x:checkedList)
        False -> getRedeclarations xs checkedList


--checks fields
checkFiltConf::[Filter]->Config->Either LexError [Filter]
checkFiltConf x conf = 
    case (checkFields conf x []) of
        [] -> Right x
        l -> Left $ MissingConfigField ("The following fields are not specified in the config file" ++ (intercalate ", " (map (getFilterName) l)) )

checkFields::Config->[Filter]->[Filter]->[Filter]
checkFields conf [] [] = [] 
checkFields conf [] notIncList = notIncList 
checkFields conf (x:xs) notIncList = 
    case fieldExists conf (getFilterName x) of
        True -> checkFields conf xs notIncList
        False -> checkFields conf xs (x:notIncList)

getFilterDefList::Filter->[FilterDef]
getFilterDefList (Filter _ fd) = fd

getFilterField::FilterDef->FilterField
getFilterField (FilterDef ff _ ) = ff

getFilterFieldStr::FilterField->String
getFilterFieldStr (FilterField s) = s

getFilterValList::FilterDef->[FilterVal]
getFilterValList (FilterDef _ fv ) = fv



--given a list of filters, makes sure each thing int he map belongs
--COULD ALSO DO TYPE CHEKING HERE, GIVEN THE SYMBOL TABLE
checkSubFieldsEx::Config->[Filter]->[(FilterName, [SubFieldName])]->[(FilterName, [SubFieldName])] 
checkSubFieldsEx conf [] [] = [] 
checkSubFieldsEx conf [] l = l
checkSubFieldsEx conf (x:xs) l = 
    do
        let fn = (getFilterName x) --first arg to subfield exists, the name of the field we are checking

        let confMap =  configToMap conf

        let submap = (M.lookup fn confMap)  --the submap. Here is a map of all the subfields the config specifes for particular field fn
        
        --list of filter definitions for that particular field. i.e., if the field is Doctor, this specifes all the lists of [ID: vals_here, etc]
        let fdefList = (getFilterDefList x)
        
        let missingSubFields = filter (not . (subFieldExists conf fn)) (map (getFilterFieldStr . getFilterField) fdefList) 
         
        
        --For each field in the list, we are going to check that it exists int he list, we are going to check 
         

       -- case (subFieldExists fn  ) of
         --   True -> 
           -- False ->

        if (missingSubFields == []) then checkSubFieldsEx conf xs l 
            else checkSubFieldsEx conf xs ((fn, missingSubFields) : l)


--checkSubFieldTypes::Config
--checkSubFieldValues

--given a list of filters, and a config, makes sure each filter is defined in the config

--checks erroneous fields
--Given a list of 


--checks erroneous subfields

