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
weed::String->(Program Annotation)->IO(Program Annotation)
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
        let symbolTableH = buildHeadSymbolTable allGroups hdr
      
        


        
        case  mapM_ (checkFilterTypes (conf) symbolTableH) [filters] of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> putStrLn "all field types check out"
        --redeclarations of foreach

        --forM_ filters $ \x -> do
            --case (checkFilterTypes conf symbolTable1 x) of
                --Left e -> hPrint stderr e
                --Right r -> putStrLn "all field types check"

        --table syntax checking

        --verify filters

        case  weedComputationList conf comps of
            Left e-> hPrint stderr e >>exitFailure
            Right r -> putStrLn r

        -- SAMPLE USES OF SYMBOL TABLE
        -- testIfSymbolTableContains symbolTable1 (Var "x")
    
        -------------------------------------------------------------------------
        ---------------- ************ typecheck computations ***********----------------
        -------------------------------------------------------------------------
       
       --build the compuation symbol table
       
        --checkComps
        --build 



        putStrLn "Weeded successfully"
        return (Program hdr docs [] (allGroups) filters (comps))

-- Make (Var Annotation) hashable
instance (Hashable (Var Annotation)) where
  hashWithSalt s (Var v a) = s + (hash v)

-- Utility test function to check if symbol table contains a key
testIfHSymbolTableContains :: HashMap.HashMap (Var Annotation) GroupType -> (Var Annotation) -> IO()
testIfHSymbolTableContains hashmap (Var v a) =
        case (HashMap.lookup (Var v a) hashmap) of
            Nothing -> hPrint stderr "nothing found!"
            Just r -> print ("Found VALUE " ++ show r ++ " for KEY " ++
                v ++ " in symboltable1")

-- Build symbol table from groups
buildHeadSymbolTable :: [(GroupDefs Annotation)] -> (Header Annotation) -> HashMap.HashMap (Var Annotation) GroupType
buildHeadSymbolTable groups (Header _ args) =
    do
        let keyValuesGroups = map (\(Group (t) (v) _) -> (v, t)) (groups)
        let keyValuesHeader = map (\(Arg (t) (v)) -> (v, t)) (args)
        (HashMap.fromList (keyValuesGroups ++ keyValuesHeader))

--buildCompSymbolTable::[Computation]->HashMap.HashMap (Var Annotation) 
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


--what is the symbol table doing exactly?
--buildBodySymbolTable::[Computation]->M.Map (Var Annotation)  Type??
--Table
--List

getGroupDefs::IO(String) -> IO([(GroupDefs Annotation)])
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
        --hPrint stderr $ M.showTree $ totalMap 
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
checkFilters::[(Filter Annotation)]->Config->Either LexError [(Filter Annotation)]
checkFilters filList conf = case (checkFilRedec filList ) of
    Right r ->
        case (checkFieldsEx conf r [] ) of
            [] -> Right filList
            l -> Left $ MissingConfigField $ "Error. Fields Missing in " ++ (M.showTreeWith (\k x -> show (k,x)) True False (M.fromList l) )
    Left e -> Left e
--Highest level, checkFilters. Is in the either monad to give us error checking

getFilterName::(Filter Annotation)->FilterName
getFilterName (Filter f _) = f

checkFilRedec::[(Filter Annotation)]->Either LexError [(Filter Annotation)]
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
checkFiltConf::[(Filter Annotation)]->Config->Either LexError [(Filter Annotation)]
checkFiltConf x conf =
    case (checkFields conf x []) of
        [] -> Right x
        l -> Left $ MissingConfigField ("The following fields are not specified in the config file" ++ (intercalate ", " (map (getFilterName) l)) )

checkFields::Config->[(Filter Annotation)]->[(Filter Annotation)]->[(Filter Annotation)]
checkFields conf [] [] = []
checkFields conf [] notIncList = notIncList
checkFields conf (x:xs) notIncList =
    case fieldExists conf (getFilterName x) of
        True -> checkFields conf xs notIncList
        False -> checkFields conf xs (x:notIncList)

getFieldDefList::(Filter Annotation)->[(FieldDef Annotation)]
getFieldDefList (Filter _ fd) = fd

getFieldName::(FieldDef Annotation)->FieldName
getFieldName (FieldDef ff _ ) = ff

getFieldValList::(FieldDef Annotation)->[(FieldVal Annotation)]
getFieldValList (FieldDef _ fv ) = fv

fieldExists::Config->FieldName->Bool
fieldExists (Config confmap) fname = (M.member (fname, True) confmap) || (M.member (fname, False) confmap) 

subFieldExists::Config->FieldName->FieldName->Bool
subFieldExists (Config confmap) fname sfname =
    case (M.lookup (fname, True) confmap) of
        Nothing -> case (M.lookup (fname, False) confmap) of
            Nothing-> False
            Just (FieldMap m) -> M.member sfname m
        Just (FieldMap m) -> M.member sfname m

--given a list of filters, makes sure each thing int he map belongs
--COULD ALSO DO TYPE CHEKING HERE, GIVEN THE SYMBOL TABLE
checkFieldsEx::Config->[(Filter Annotation)]->[(FilterName, [FilterName])]->[(FilterName, [FilterName])]
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

--filter
--give hmap from var to grouptype
--give conf
--GOAL: check that the types of the filter
--phase 1: check to see that all
checkFilterTypes::Config->(HashMap.HashMap (Var Annotation) GroupType)->[(Filter Annotation)]->Either LexError ()--[(Filter Annotation)]
checkFilterTypes (Config conf) hmap ms =
    do
        forM_ ms $ \x -> do
            --from fields
            let filterName = (getFilterName x)
            let fieldDefs = getFieldDefList x -- list of possible FieldDefs for a filter
            --from confmap
            case (M.lookup (filterName, True) conf) of--things to check against for that filter
                Nothing -> case (M.lookup (filterName, False) conf) of
                    Nothing -> Left $ GenError (filterName ++ " is not in map")     
                    Just val -> (typeCheckFieldMap val hmap) fieldDefs

                Just val ->  (typeCheckFieldMap val hmap) fieldDefs

--return a list of things that don't type check
typeCheckFieldMap::FieldMap->(HashMap.HashMap (Var Annotation) GroupType)->[(FieldDef Annotation)]->Either LexError () --[FieldDef] 
typeCheckFieldMap (FieldMap fm) hmap fdList = do
   forM_ fdList $ \x ->
        do
            let fieldName = getFieldName x
            let fvalList  = getFieldValList x
            case (M.lookup fieldName fm) of
                Nothing -> Left $ GenError "Not somethign"
                Just val -> mapM_ (compareFieldTypes val (FieldMap fm) hmap) fvalList

--take fieldDefs anda  fieldMap, return an error or a field deaf after calling Field
compareFieldTypes::Field->(FieldMap)->(HashMap.HashMap (Var Annotation) GroupType)->(GroupItem Annotation)->Either LexError ()--GroupItem
compareFieldTypes (FieldValue allValList) fm hm (GroupValString s _)  = 
    if (s `elem` allValList) then Right ()--Right (GroupValString s) 
    else Left (AllowedValError ("Error. " ++ s ++ " is not defined in the config file list that also contains: " ++ (intercalate "," allValList)))
compareFieldTypes (FieldType "String") fm hm (GroupValString s _)  = Right ()
compareFieldTypes (FieldType "Int") fm hm (GroupRange _) = Right ()
compareFieldTypes (FieldType "Date") fm hm (GroupDate _ _ _) = Right ()
--I need a new type that allows me to pull out the type of var 
compareFieldTypes _ (FieldMap fm) hm  (GroupVar var) =
    case (HashMap.lookup var hm) of
        Nothing -> Left $ TypeError ("ERROR. var " ++ (varToStr var) ++ "not declared")
        Just a -> case (M.member ((groupTypeToStr a)) fm) of --if it is declared, we want to make sure it is used in the right case. i.e. with the right field
                False -> Left $ TypeError ("ERROR. variable " ++ (varToStr var) ++" is used with wrong field")
                True  -> Right () 
compareFieldTypes b fm hm a = Left $ TypeError ("Type Error between " ++ (show a) ++ " and " ++ (show b))

varToStr::(Var Annotation)->String
varToStr (Var v _) = v

groupTypeToStr::GroupType->String
groupTypeToStr (GroupType s) = s


events :: [String]
events = ["consult_referral_received","initial_consult_booked","initial_consult_completed",
            "ct_sim_booked","ready_for_ct_sim","ct_sim_completed","ready_for_initial_contour","ready_for_md_contour",
            "ready_for_dose_calculation","prescription_approved_by_md","ready_for_physics_qa","ready_for_treatment",
            "machine_rooms_booked","patient_contacted","end"]

weedComputationList :: Config->[(Computation Annotation)]->Either LexError String
weedComputationList config comps = 
    do
    
        let compSymbolTable = [emptyScope]
        case (weedFold config compSymbolTable comps) of
            Right r -> Right  r--(trace ("OH" ++ (show r)) r )
            Left e -> Left e 

weedFold :: Config->CompSymTable -> [(Computation Annotation)] -> Either LexError String
weedFold conf symtable computations = 
    do
        case mapM (weedEach conf symtable) computations of
            Right r -> Right $ printFold ((concat r) )
            Left l -> Left l
        --res <-  printFold (foldl' (weedEach conf) symtable computations)
        --return res
            --Left e -> Left e
            --Right r -> Right r

weedEach::Config->CompSymTable->(Computation Annotation)->Either LexError CompSymTable
weedEach conf sym comp =  case (weedAndTypeCheckComp conf sym comp) of
    Left x ->  Left x --trace ("Error! "++ (show x) ++ " occured but not handled") sym
    Right x -> Right x

printFold :: CompSymTable -> String
printFold symtable = 
    let 
        len = show $ length symtable
        curr= last symtable
    in HashMap.foldrWithKey  (\(Var s _) t p -> p ++ s ++ "\t" ++ (tail $ show t) ++"\t" ++len++ "\n" )  "" curr



addToSymTable :: CompSymTable -> (Var Annotation) -> ComputationType-> CompSymTable
addToSymTable symtable v  comptype = 
    let prev= init symtable
        local = last symtable
        updated = HashMap.insert  v comptype local
    in prev++[updated]

type Scope = HashMap.HashMap (Var Annotation) ComputationType

type CompSymTable = [Scope]

getFromScope:: Scope ->(Var Annotation)-> Maybe ComputationType
getFromScope  hashmap v = (HashMap.lookup v hashmap)

getFromSymbolTable :: CompSymTable -> (Var Annotation) -> Maybe ComputationType
getFromSymbolTable  sym v = 
    foldr fun Nothing sym  
    where fun  scope prev = case (prev) of
                            Nothing -> getFromScope scope v
                            _ -> prev

isNowInTopScope::CompSymTable -> Bool
isNowInTopScope  symtable  = trace (show symtable) (null $ tail symtable) -- && (elem v $ testIfScopeContains $ head symtable v)

--evaluateInTopScope :: CompSymTable
evaluateInTopScope symtable f = if isNowInTopScope symtable
    then f symtable
    else Left $ ComputationWrongScope "Can only be in top scope"

isInLoopableScope = error


emptyScope :: HashMap.HashMap (Var Annotation) ComputationType
emptyScope = HashMap.fromList []

 
weedAndTypeCheckComp :: Config->CompSymTable -> (Computation Annotation)-> Either LexError CompSymTable
weedAndTypeCheckComp conf symtable  (Table variable constructor  field) =
    evaluateInTopScope symtable fun 
    where fun sym = if ((subFieldExists conf constructor field))
            then Right $ addToSymTable sym  variable TTable --(TFilter constructor)
            else Left . FieldNameError $ "Field "++field++
             " does not belong to " ++ constructor 
weedAndTypeCheckComp conf symtable (List variable seqlist) =  
    evaluateInTopScope symtable fun 
    where fun sym = foldl' foldWeedList (Right $ addToSymTable sym  variable TList) seqlist
weedAndTypeCheckComp conf symtable (Barchart variable) = 
    evaluateInTopScope symtable (\sym->
        case getFromSymbolTable sym variable of
            Nothing -> Left . UndefinedVariable $ show  variable 
            Just t -> if t == TTable 
                then Right symtable
                else Left $ ComputationTypeMismatch $ 
                    "Cannot draw Barchart of "++ (show variable)++". It is a " ++ (show t) ++ "Not a Table"
                ) 

weedAndTypeCheckComp conf symtable (Print printAction) = weedPrintAction  symtable printAction
weedAndTypeCheckComp conf symtable (Foreach def comps) = weedForEach conf symtable comps def

weedPrintAction :: CompSymTable -> (PrintAction Annotation)-> Either LexError CompSymTable
weedPrintAction symtable (PrintVar var) = case getFromSymbolTable symtable var of
            Nothing -> Left . UndefinedVariable $ show  var 
            Just t -> Right symtable
weedPrintAction symtable (PrintLength variable) = case getFromSymbolTable symtable variable of
            Nothing -> Left . UndefinedVariable $ show  variable 
            Just t -> if t == TTable 
                then Right symtable
                else Left . ComputationTypeMismatch $ 
                    "Cannot have length of "++ (show variable)++". It is a " ++ (show t) ++ "Not a Table"
                
weedPrintAction symtable printAction = Left $ ComputationWrongScope "Unimplemented"

weedForEach::Config->CompSymTable -> [(Computation Annotation)] ->(ForEachDef Annotation)-> Either LexError CompSymTable
weedForEach config symtable newcomp (ForEachFilter filterName var )  = 
    if (fieldExists config filterName) 
    then    if (isValidInNestedLoopables config symtable filterName)
            then 
                do
                    let newsym =(addToSymTable (symtable++[emptyScope]) var (TFilter filterName))
                    case (weedFold config newsym newcomp) of
                            Right r -> Right symtable 
                            Left e -> Left e 
            else Left $  ComputationWrongScope "Foreach is not valid in this scope"
    else Left $  FieldNameError $ "\""++filterName++"\" is not a valid loopable Filter"

weedForEach config symtable newcomp (ForEachTable indexVar tableVar)  = evaluateInTopScope symtable (\sym->
        case getFromSymbolTable sym tableVar of
            Nothing -> Left . UndefinedVariable $ show  tableVar 
            Just t -> if t == TTable 
                then 
                    do 
                        let newsym =(addToSymTable (symtable++[emptyScope]) indexVar TIndex)
                        case (weedFold config newsym newcomp) of
                            Right r -> Right symtable 
                            Left e -> Left e 
                else Left . ComputationTypeMismatch $ 
                    "CAnnot Go through loop for "++ (show tableVar)++". It is a " ++ (show t) ++ "Not a Table"
                ) 


weedForEach config symtable newcomp (ForEachSequence memberVar unusedSequence)  = Left $ ComputationWrongScope "Unimplemented"
weedForEach config symtable newcomp (ForEachList memberVar listVar)  = evaluateInTopScope symtable (\sym->
        case getFromSymbolTable sym listVar of
            Nothing -> Left . UndefinedVariable $ show  listVar 
            Just t -> if t == TList 
                then 
                    do
                        let newsym =(addToSymTable (symtable++[emptyScope]) memberVar TSequence)
                    
                        case (weedFold config newsym newcomp) of
                            Right r -> Right symtable 
                            Left e -> Left e 
                else Left . ComputationTypeMismatch $ 
                    "CAnnot Go through loop for "++ (show listVar)++". It is a " ++ (show t) ++ "Not a List"
                ) 

weedSequence :: (SeqField Annotation)-> Either String Bool
weedSequence (Bar evlist) = checkEvents evlist
weedSequence (Comma evlist) = checkEvents evlist
weedSequence (Star evlist) = checkEvents evlist
weedSequence (Neg (ev)) = checkEvents [ev]

checkEvents :: [(Event Annotation)] -> Either String Bool
checkEvents evlist = foldl' (\prev (Event curr a) -> 
        if ( elem curr events) 
        then prev 
        else Left curr) 
    (Right True) evlist
foldWeedList :: (Either LexError CompSymTable) -> (SeqField Annotation) -> (Either LexError CompSymTable)
foldWeedList prev curr = 
    case weedSequence curr of  
        Left evname -> Left $ IncorrectEvent evname
        _-> prev


isValidInNestedLoopables ::Config-> CompSymTable -> FilterName -> Bool
isValidInNestedLoopables conf symtable filterName = 
    let 
        (counts, filtersUsed) = unzip (findAllFilters symtable)
    in ((null filtersUsed) || ( not (elem filterName filtersUsed)))


findAllFilters :: CompSymTable -> [(Int,FilterName)]
findAllFilters symtable = foldl' (\ p  (c,s)-> let f =find1Filter s
                                        in if (not $ null f)  
                                            then p++[(c,f)]
                                            else p) [] (zip [1..] symtable)
find1Filter :: Scope -> FilterName
find1Filter curr = HashMap.foldr  (\ t p -> if (null p)
                                    then case t of 
                                        TFilter val -> val
                                        _ -> p
                                    else p)  "" curr 

