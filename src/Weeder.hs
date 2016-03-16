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
        putStrLn "ohai"
        --get Config file
        conf <- readConfig file
        putStrLn $ show conf
        putStrLn $ "File "++file++"\n"
       --grpFile weeding
        curContents <- (getDirectoryContents  $ dropFileName file)
        let dirContents = curContents -- ++ valContents ++ invContents
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles
        let grpFileList = weedGroupFiles useList grpFileNames
        let useFilesToParse = map (\x -> "programs/valid/" ++ x ++ ".grp") (flattenUseFile useList)

        --putStrLn ("Group files are " ++ (show useFilesToParse))
        case grpFileList of
            Left e -> hPutStrLn stderr (file ++ ": ") >> print e >> exitFailure
            Right r -> putStrLn $ file ++ ": All Group files exist"

        --parsing each group file
        let grpAllFilesContents = map (readFile) (useFilesToParse)
        newGroups <- sequence (map (getGroupDefs) (grpAllFilesContents))

        --check erroneous subfields i.e. whether all fields exist

        case (checkFilters filters conf) of
            Left e -> print e >>  hPutStrLn stderr "FILTERS:" >> print filters >> putStrLn "CONF:" >> print conf >>  exitFailure
            Right r -> putStrLn "All Fields valid"


        let allGroups = (concat (newGroups)) ++ groupDefs
        let symbolTable1 = buildSymbolTable allGroups hdr


        case  mapM_ (checkFilterTypes (conf) symbolTable1) [filters] of
            Left e -> (hPutStrLn stderr (show e)) >> exitFailure
            Right r -> putStrLn "all field types check out"
        --redeclarations of foreach

        --forM_ filters $ \x -> do
            --case (checkFilterTypes conf symbolTable1 x) of
                --Left e -> print e
                --Right r -> putStrLn "all field types check"

        --table syntax checking

        --verify filters

        putStrLn $ weedComputationList comps

        -- SAMPLE USES OF SYMBOL TABLE
        -- testIfSymbolTableContains symbolTable1 (Var "x")

        putStrLn "Weeded successfully"
        return (Program hdr docs [] (allGroups) filters (comps))

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
            Left e -> hPutStrLn stderr ("ERROR: " ++ show e) >> return []
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
        case (checkFieldsEx conf r [] ) of
            [] -> Right filList
            l -> Left $ MissingConfigField $ "Error. Fields Missing in " ++ (M.showTreeWith (\k x -> show (k,x)) True False (M.fromList l) )
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

getFieldDefList::Filter->[FieldDef]
getFieldDefList (Filter _ fd) = fd

getFieldName::FieldDef->FieldName
getFieldName (FieldDef ff _ ) = ff

getFieldValList::FieldDef->[FieldVal]
getFieldValList (FieldDef _ fv ) = fv

fieldExists::Config->FieldName->Bool
fieldExists (Config confmap) fname = M.member fname confmap

subFieldExists::Config->FieldName->FieldName->Bool
subFieldExists (Config confmap) fname sfname =
    case (M.lookup fname confmap) of
        Nothing -> False
        Just (FieldMap m) -> M.member sfname m

--given a list of filters, makes sure each thing int he map belongs
--COULD ALSO DO TYPE CHEKING HERE, GIVEN THE SYMBOL TABLE
checkFieldsEx::Config->[Filter]->[(FilterName, [FilterName])]->[(FilterName, [FilterName])]
checkFieldsEx conf [] [] = []
checkFieldsEx conf [] l = l
checkFieldsEx conf (x:xs) l =
    do
        let fn = (getFilterName x) --first arg to subfield exists, the name of the field we are checking
        --let confMap =  configToMap conf

        --list of filter definitions for that particular field. i.e., if the field is Doctor, this specifes all the lists of [ID: vals_here, etc]
        let fdefList = (getFieldDefList x)
        let missingFields = filter (not . (subFieldExists conf fn)) (map (getFieldName) fdefList)
        --For each field in the list, we are going to check that it exists int he list, we are going to check
        if (missingFields == []) then checkFieldsEx conf xs l
            else checkFieldsEx conf xs ((fn, missingFields) : l)

--checkFieldTypes::Config
--checkFieldValues

--typeCheckFilters::Config->(HashMap.HashMap Var GroupType)->[Filter]->Either LexError [Filter]
--typeCheckFilters conf hmap flist = case checkFieldTypes )of

--filter
--give hmap from var to grouptype
--give conf
--GOAL: check that the types of the filter
--phase 1: check to see that all
checkFilterTypes::Config->(HashMap.HashMap Var GroupType)->[Filter]->Either LexError ()--[Filter]
checkFilterTypes (Config conf) hmap ms =
    do
        forM_ ms $ \x -> do
            --from fields
            let filterName = (getFilterName x)
            let fieldDefs = getFieldDefList x -- list of possible FieldDefs for a filter
            --from confmap
            case (M.lookup filterName conf) of--things to check against for that filter
                Nothing -> Left $ GenError "not in map"
                Just val ->  (typeCheckFieldMap val) fieldDefs

--return a list of things that don't type check
typeCheckFieldMap::FieldMap->[FieldDef]->Either LexError () --[FieldDef]
typeCheckFieldMap (FieldMap fm)  fdList = do
    forM_ fdList $ \x ->
        do
            let fieldName = getFieldName x
            let fvalList  = getFieldValList x
            case (M.lookup fieldName fm) of
                Nothing -> Left $ GenError "Not somethign"
                Just val -> mapM_ (compareFieldTypes val) fvalList


--take fieldDefs anda  fieldMap, return an error or a field deaf after calling Field
compareFieldTypes::Field->GroupItem->Either LexError ()--GroupItem
compareFieldTypes (FieldValue allValList)(GroupValString s)  =
    if (s `elem` allValList) then Right ()--Right (GroupValString s)
    else Left (AllowedValError ("Error. " ++ s ++ " is not defined in the config file"))
compareFieldTypes (FieldType "String")g@(GroupValString _)  = Right ()
compareFieldTypes (FieldType "Int") g@(GroupRange _) = Right ()
compareFieldTypes (FieldType "Date") g@(GroupDate _ _ _) = Right ()
compareFieldTypes b a = Left $ TypeError ("Type Error between " ++ (show a) ++ " and " ++ (show b))






weedComputationList :: [Computation]->String
weedComputationList _ = let {
correct1=[Table (Var "t") "patients" (FilterField "birthyear"),Print (PrintLength (Var "t")),
Foreach (ForEachFilter "patient" (Var "p")) [Print (PrintFilters ["sex","postal_code"] (Var "p")),
Foreach (ForEachFilter "doctor" (Var "d")) [Foreach (ForEachFilter "diagnosis" (Var "i")) []]],
List (Var "s") [Bar [Event "ct_sim_completed"],Bar [Event "ct_sim_booked"],Bar [Event "treatment_began"],
Bar [Event "consult_referral_received"]],Foreach (ForEachList (Var "i") (Var "s")) [Print (PrintVar (Var "i"))]];
correct2 =  [List (Var "s") [Single (Event "ct_sim_completed"),Single (Event "ct_sim_booked"),
Single (Event "treatment_began"),Single (Event "consult_referral_received")],Foreach (ForEachList (Var "i") (Var "s")) [Barchart (Var "i")]];


t=Table (Var "t") "patients" (FilterField "birthyear");

l=List (Var "s") [Bar [Event "ct_sim_completed"],Bar [Event "ct_sim_booked"],Bar [Event "ready_for_treatment"],
Bar [Event "consult_referral_received"]];

compSymbolTable=[emptyScope];
x=(weedFold compSymbolTable [l]);
s = show x
} in trace s x 

weedFold symtable computations = printFold (foldl' weedEach symtable computations)
weedEach sym comp =  case (weedAndTypeCheckComp sym comp) of
    Left x ->  error $ show  x
    Right x -> x
printFold symtable = 
    let 
        len = show $ length symtable
        curr= last symtable
    in HashMap.foldrWithKey  (\(Var s) t p -> p ++ s ++ "\t" ++ (tail $ show t) ++"\t" ++len++ "\n" )  "" curr
 

loopables:: Config
loopables = Config (M.fromList [("diagnosis",SubMap (M.fromList [("name",("string",[]))])),
    ("doctor",SubMap (M.fromList [("id",("string",[])),("oncologist",("string",["true","false"]))])),
    ("doctors",SubMap (M.fromList [("id",("string",[])),("oncologist",("string",["true","false"]))])),
    ("patient",SubMap (M.fromList [("birthyear",("string",[])),("diagnosis",("string",[])),
        ("gender",("string",[])),("id",("string",[])),("postalcode",("string",[]))])),
    ("patients",SubMap (M.fromList [("birthyear",("string",[])),("diagnosis",("string",[])),
        ("gender",("string",[])),("id",("string",[])),("postalcode",("string",[]))]))])

events :: [String]
events = ["consult_referral_received","initial_consult_booked","initial_consult_completed",
            "ct_sim_booked","ready_for_ct_sim","ct_sim_completed","ready_for_initial_contour","ready_for_md_contour",
            "ready_for_dose_calculation","prescription_approved_by_md","ready_for_physics_qa","ready_for_treatment",
            "machine_rooms_booked","patient_contacted","end"]
--

addToSymTable :: CompSymTable -> Var -> ComputationType-> CompSymTable
addToSymTable symtable v  comptype = 
    let prev= init symtable
        local = last symtable
        updated = HashMap.insert  v comptype local
    in prev++[updated]

type Scope = HashMap.HashMap Var ComputationType
type CompSymTable = [Scope]
testIfScopeContains:: Scope ->Var-> Maybe ComputationType
testIfScopeContains  hashmap v = (HashMap.lookup v hashmap)

isNowInTopScope::CompSymTable -> Bool
isNowInTopScope  symtable  = (null $ tail symtable) -- && (elem v $ testIfScopeContains $ head symtable v)

isInLoopableScope = error

--checks erroneous subfields
emptyScope :: HashMap.HashMap Var ComputationType
emptyScope = HashMap.fromList []

--type symbol=[HashMap.HashMap Var ComputationType]

isInScope :: [HashMap.HashMap Var ComputationType] -> Var -> Maybe ComputationType
isInScope xlist var = 
    case xlist of 
        []->Nothing
        (x:xs) -> 
            case (isInScope xs var) of
                Nothing -> testIfScopeContains x var
                r -> r
 
weedAndTypeCheckComp :: CompSymTable -> Computation -> Either LexError CompSymTable
weedAndTypeCheckComp symtable  (Table variable constructor (FilterField field)) =
    if isNowInTopScope symtable
    then if (trace "looking in loopables" (subFieldExists loopables constructor field))
            then Right $ addToSymTable symtable  variable TTable --(TFilter constructor)
            else Left $ SubFieldNameError $ "Subfield "++field++ " does not belong to " ++ constructor   
    else Left $ ComputationWrongScope "Can only be in top scope"

weedAndTypeCheckComp symtable (List variable seqlist) = 
    if isNowInTopScope symtable
    then foldl' foldWeedList (Right $ addToSymTable symtable  variable TList) seqlist
    else Left $ ComputationWrongScope "Can only be in top scope"
weedAndTypeCheckComp symtable _ = Left $ ComputationWrongScope "Unimplemented"

weedSequence :: SeqField -> Either String Bool
weedSequence (Bar evlist) = foldl' (\prev (Event curr) -> if ( elem curr events) then prev else Left curr) (Right True) evlist
weedSequence (Comma evlist) = foldl' (\prev (Event curr) -> if ( elem curr events) then prev else Left curr) (Right True) evlist
weedSequence (Star evlist) = foldl' (\prev (Event curr) -> if ( elem curr events) then prev else Left curr) (Right True) evlist
weedSequence (Neg (Event curr)) = if (elem curr events) then (Right True) else Left curr

foldWeedList :: (Either LexError CompSymTable) -> SeqField -> (Either LexError CompSymTable)
foldWeedList prev curr = 
    case weedSequence curr of  
        Left evname -> Left $ IncorrectEvent evname
        _-> prev

