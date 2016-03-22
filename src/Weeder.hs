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
weed::String->(String -> IO())->(Program Annotation)->IO(Program Annotation)
weed file symTabFun prg@(Program hdr docs useList groupDefs filters comps) =
    do
        --get Config file
        conf <- readConfig file
        -- putStrLn $ show conf
        putStrLn $ "File "++file++"\n"
       --grpFile weeding
        curContents <- (getDirectoryContents  $ dropFileName file)
        let dirContents = curContents -- ++ valContents ++ invContents
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles
        let grpFileList = weedGroupFiles useList grpFileNames
        let useFilesToParse = map (\x -> "programs/valid/" ++ x ++ ".grp") (flattenUseFile useList)

        let (Config confmap) = conf
            loopable = Config $ M.filterWithKey (\(name,valid) t -> valid) confmap
            filterable = Config $ M.filterWithKey (\(name,valid) t -> not valid) confmap

        --putStrLn ("Group files are " ++ (show useFilesToParse))
        case grpFileList of
            Left e -> hPutStrLn stderr (file ++ ": ") >> print e >> exitFailure
            Right r -> putStrLn $ file ++ ": All Group files exist"

        --parsing each group file
        let grpAllFilesContents = map (readFile) (useFilesToParse)
        newGroups <- sequence (map (getGroupDefs) (grpAllFilesContents))

        --check erroneous subfields i.e. whether all fields exist
        case (checkFilters filters filterable) of
            Left e -> print e >>  hPutStrLn stderr "FILTERS:" >>
                print filters >> putStrLn "CONF:" >> print filterable >> exitFailure
            Right r -> putStrLn "All Fields valid"

        let allGroups = (concat (newGroups)) ++ groupDefs

        -- check for redeclarations for group
        case (checkForGroupRedecl allGroups) of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> print "No redeclarations for group files"

        -- take care of recursive groups
        case (checkForRecursiveGroups allGroups) of
            [] -> print "No recursively defined groups"
            vars -> hPrint stderr (RecursiveGroups ("Following group vars " ++
                "recursively defined: " ++ varsToStr (vars))) >> exitFailure

        -- check if variable being used in group is actually defined in symbol table #83
        -- check types of groups and if they exist #99
        let symbolTableH = buildHeadSymbolTable allGroups hdr

        case  mapM_ (checkFilterTypes conf symbolTableH) [filters] of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> putStrLn "all field types check out"
        --case  mapM_ (checkFilterTypes (filterable) symbolTableH) [filters] of

        -------- TEST ---------

        print "Printing old filters"

        print filters

        print "Printing new filters"

        let newFilters = (populateDefaultValuesFilters filters (Config confmap))

        print newFilters

        -- Replace vars with symbol table h
        print "Printing with vars replaced"
        print (map (replaceVarsFilter symbolTableH) newFilters)

        -------- UNTEST -------

        case (M.lookup ("patient", True) confmap) of
            Nothing -> hPrint stderr "key not found in confmap!"
            Just (FieldMap r) -> print "here's a confmap" >> print (show (M.toList r))

        case  weedComputationList conf comps of
            Left e-> hPrint stderr e >>exitFailure
            Right r -> symTabFun r

        -- SAMPLE USES OF SYMBOL TABLE
        -- testIfSymbolTableContains symbolTable1 (Var "x")

        -------------------------------------------------------------------------
        ---------------- ************ typecheck computations ***********----------------
        -------------------------------------------------------------------------

       --build the compuation symbol table

        --checkComps
        --build



        -- print (show (prg))

        putStrLn "Weeded successfully"
        return (Program hdr docs [] (allGroups) filters (comps))


checkForRecursiveGroups :: [GroupDefs Annotation] -> [Var Annotation]
checkForRecursiveGroups groups =
    do
        foldl (\recGrps curGrp -> case curGrp of
                Left var -> (var : recGrps)
                Right var -> (recGrps)
                ) [] (map checkForRecursiveEachGroup groups)

checkForRecursiveEachGroup :: (Eq a) => GroupDefs a -> Either (Var a) (Var a)
checkForRecursiveEachGroup (Group t var items) =
    do
        case (GroupVar var) `elem` items of
            True -> Left $ var
            False -> Right $ var

checkForGroupRedecl :: [GroupDefs Annotation] -> Either LexError [GroupDefs Annotation]
checkForGroupRedecl groups =
    do
        -- traverse list and make sure no group has group var the same
        -- get list of vars
        let varList = map (\(Group t v _) -> v) groups
        -- check for dups
        case (dupesExist varList) of
            (_, []) -> Right $ groups
            (_, repeated) -> Left $ RedecError ("The following groups were redeclared: "
                ++ (intercalate ", " (map (varToStr) repeated)))

dupesExist :: [Var Annotation] -> ([String], [Var Annotation])
dupesExist vars =
    do
        -- length (nubBy (\(Var x) (Var y) -> x == y) vars) == length (vars)
        foldl (\(seenVars, repeated) (Var x a) ->
            if (x `elem` seenVars) then (x : seenVars, (Var x a) : repeated) else
                (x : seenVars, repeated)) ([], []) vars

replaceVarsFilter :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> Filter Annotation -> Filter Annotation
replaceVarsFilter symbolTableH (Filter fname fdefs) =
    do
        Filter fname (map (replaceVarsFieldDef symbolTableH) fdefs)

replaceVarsFieldDef :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> FieldDef Annotation -> FieldDef Annotation
replaceVarsFieldDef symbolTableH (FieldDef fname fvalues) =
    do
        let varList = filter (\v -> case v of
                GroupVar x -> True
                _ -> False) fvalues
        let nonVarList = filter (\v -> case v of
                GroupVar x -> False
                _ -> True) fvalues
        let expandedList = concat (map (replaceVarsGroupItem symbolTableH) varList)
        FieldDef fname (nonVarList ++ expandedList)

replaceVarsGroupItem :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> GroupItem Annotation -> [GroupItem Annotation]
replaceVarsGroupItem symbolTableH (GroupVar v) =
    do
        case (HashMap.lookup v symbolTableH) of
            Just (t, items) -> items

-- Make (Var Annotation) hashable
instance (Hashable (Var Annotation)) where
  hashWithSalt s (Var v a) = s + (hash v)

-- Utility test function to check if symbol table contains a key
testIfHSymbolTableContains :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> (Var Annotation) -> IO()
testIfHSymbolTableContains hashmap (Var v a) =
        case (HashMap.lookup (Var v a) hashmap) of
            Nothing -> hPrint stderr "nothing found!"
            Just r -> print ("Found VALUE " ++ show r ++ " for KEY " ++
                v ++ " in symboltable1")

-- Build symbol table from groups
buildHeadSymbolTable :: [(GroupDefs Annotation)] -> (Header Annotation) -> HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation])
buildHeadSymbolTable groups (Header _ args) =
    do
        let keyValuesGroups = map (\(Group (t) (v) items) -> (v, (t, items))) (groups)
        let keyValuesHeader = map (\(Arg (t) (v)) -> (v, (t, []))) (args)
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
                    False -> Left $ MissingFilesError (
                        "ERROR: Missing one of group files: "
                        ++ ( intercalate ","  declaredUseFiles) ++
                        " out of: " ++ (intercalate "," grpFiles))
                    --Better error messages for other cases. Maybe see what files
                    -- are missing exactly. Doesn't need to be true false exactly
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

readConfig::String->IO((Config Annotation))
readConfig file =
    do
        program <- readFile file
        readData <- readFile "config.conf"
        let l= lines readData
        let totalMap = configListToMap $ map makeConfig l
        --hPrint stderr $ M.showTree $ totalMap
        --print $ M.showTree $ totalMap
        return $ Config totalMap

-- use config to populate default values for a filter
populateDefaultValuesFilters :: [Filter Annotation] -> Config -> [Filter Annotation]
populateDefaultValuesFilters filters config =
    do
        map (findDefaultValuesFilt config) (filters)

findDefaultValuesFilt :: Config -> Filter Annotation -> Filter Annotation
findDefaultValuesFilt (Config conf) (Filter fname defs) =
    do
        -- get all fields for a filter from config
        let fieldMap = case (M.lookup (fname, False) conf) of
                -- Nothing -> error
                Just r -> r
            -- Just (FieldMap r) -> (M.toList r)
        let fieldsFromFieldMap = (fieldMapToFieldDefs fieldMap)
        let newDefs = unionBy
                (\ (FieldDef xname _) (FieldDef yname _) ->
                    xname == yname)
                defs
                fieldsFromFieldMap
        Filter fname (newDefs)

fieldMapToFieldDefs :: FieldMap -> [FieldDef Annotation]
fieldMapToFieldDefs (FieldMap fmap) =
    do
        let flist = M.toList fmap
        map (\(name, field) -> FieldDef name [GroupWildcard]) flist



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
            --Better error messages for other cases. Maybe see what files are missing exactly.
            --Doesn't need to be true false exactly
            True -> Right $ useFiles
checkFilters::[(Filter Annotation)]->(Config Annotation)->Either LexError [(Filter Annotation)]
checkFilters filList conf = case (checkFilRedec filList ) of
    Right r ->
        case (checkFieldsEx conf r [] ) of
            [] -> Right filList
            l -> Left $ MissingConfigField $ "Error. Fields Missing in " ++
                    (M.showTreeWith (\k x -> show (k,x)) True False (M.fromList l) )
    Left e -> Left e
--Highest level, checkFilters. Is in the either monad to give us error checking

getFilterName::(Filter Annotation)->FilterName
getFilterName (Filter f _) = f

checkFilRedec::[(Filter Annotation)]->Either LexError [(Filter Annotation)]
checkFilRedec [] = Right []
checkFilRedec x =
    case (getRedeclarations x []) of
        [] -> Right $ x
        y -> Left $ RedecError ("The following filters were redeclared: "
            ++ (intercalate ", " (map (getFilterName) y)) )
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
checkFiltConf::[(Filter Annotation)]->(Config Annotation)->Either LexError [(Filter Annotation)]
checkFiltConf x conf =
    case (checkFields conf x []) of
        [] -> Right x
        l -> Left $ MissingConfigField (
            "The following fields are not specified in the config file"
            ++ (intercalate ", " (map (getFilterName) l)) )

checkFields::(Config Annotation)->[(Filter Annotation)]->[(Filter Annotation)]->[(Filter Annotation)]
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

fieldExists::(Config Annotation)->FieldName->Bool
fieldExists (Config confmap) fname = (M.member (fname, True) confmap)
                                        || (M.member (fname, False) confmap)

subFieldExists::(Config Annotation)->FilterName->FieldName->Bool
subFieldExists (Config confmap) fname sfname =
    case (M.lookup (fname, True) confmap) of
        Nothing -> case (M.lookup (fname, False) confmap) of
            Nothing-> False
            Just (FieldMap m) -> M.member sfname m
        Just (FieldMap m) -> M.member sfname m

--given a list of filters, makes sure each thing int he map belongs
--COULD ALSO DO TYPE CHEKING HERE, GIVEN THE SYMBOL TABLE
checkFieldsEx::(Config Annotation)->[(Filter Annotation)]->[(FilterName, [FilterName])]->[(FilterName, [FilterName])]
checkFieldsEx conf [] [] = []
checkFieldsEx conf [] l = l
checkFieldsEx conf (x:xs) l =
    do
        let fn = (getFilterName x) --first arg to field exists, the name of the field we are checking
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
checkFilterTypes::(Config Annotation)->(HashMap.HashMap (Var Annotation) GroupType)->[(Filter Annotation)]->Either LexError [(Filter Annotation)] -- ()--[(Filter Annotation)]
checkFilterTypes (Config conf) hmap ms =
    do
--How can I accumulate stuff?
        
       -- forM_ ms  (\x -> do
        foldM (\acc x-> do
            --from fields
            let filterName = (getFilterName x)
            let fieldDefs = getFieldDefList x -- list of possible FieldDefs for a filter
            --from confmap
            case (M.lookup (filterName, True) conf) of--things to check against for that filter
                Just val ->  case ((typeCheckFieldMap val hmap) fieldDefs) of
                    Left e -> Left e
                    Right r ->  Right $ (Filter filterName r):acc
                Nothing -> case (M.lookup (filterName, False) conf) of
                    Nothing -> Left $ GenError (filterName ++ " is not in map")
                    Just val -> case ((typeCheckFieldMap val hmap) fieldDefs) of
                        Left e -> Left e
                        Right r -> Right $ (Filter filterName r ):acc
                ) [] ms 

--return a list of things that don't type check

--NEED TO RETURN ANNOTATED FIELD MAP MAYBE?
typeCheckFieldMap::(FieldMap Annotation)->(HashMap.HashMap (Var Annotation) GroupType)->[(FieldDef Annotation)]->Either LexError [(FieldDef Annotation)] -- () --[FieldDef] 
typeCheckFieldMap (FieldMap fm) hmap fdList = do
   foldM (\acc x-> do 
            let fieldName = getFieldName x
            let fvalList  = getFieldValList x
            case (M.lookup fieldName fm) of
                Nothing -> Left $ GenError "Not somethign"
                Just val -> case (mapM (compareFieldTypes val (FieldMap fm) hmap) fvalList) of
                    Left e -> Left e
                    Right r -> Right $ (FieldDef fieldName r):acc
    ) [] fdList

--take fieldDefs anda  fieldMap, return an error or a field deaf after calling Field
compareFieldTypes::(Field Annotation)->(FieldMap Annotation)->(HashMap.HashMap (Var Annotation) GroupType)->(GroupItem Annotation)->Either LexError (GroupItem Annotation)
compareFieldTypes (FieldValue allValList an ) fm hm (GroupValString s a)  = 
    if (s `elem` allValList) then Right (GroupValString s an) --Right (GroupValString s) 
    else Left (AllowedValError ("Error. " ++ s ++ " is not defined in the config file list that also contains: " ++ (intercalate "," allValList)))
compareFieldTypes (FieldType "String" an) fm hm (GroupValString s a)  = Right (GroupValString s (an))
compareFieldTypes (FieldType "Int" an) fm hm (GroupRange a ) = Right (GroupRange a)
compareFieldTypes (FieldType "Date" an) fm hm (GroupDate a b c _) = Right (GroupDate a b c (an))
--I need a new type that allows me to pull out the type of var
compareFieldTypes _ (FieldMap fm) hm  (GroupVar var@(Var v an) ) =
    case (HashMap.lookup var hm) of
        Nothing -> Left $ TypeError ("ERROR. var " ++ (varToStr var) ++ " not declared")
        Just (a, _) -> case (M.member ((groupTypeToStr a)) fm) of
        --if it is declared, we want to make sure it is used in the right case. i.e. with the right field
                False -> Left $ TypeError ("ERROR. variable " ++
                    (varToStr var) ++" is used with wrong field")
                True  -> Right (GroupVar var)
compareFieldTypes b fm hm a = Left $ TypeError ("Type Error between " ++ 
    (show a) ++ " and " ++ (show b))

varToStr::(Var Annotation)->String
varToStr (Var v _) = v

varsToStr :: [Var Annotation] -> String
varsToStr vars = intercalate ", " (map (varToStr) vars)

groupTypeToStr::GroupType->String
groupTypeToStr (GroupType s) = s


events :: [String]
events = ["consult_referral_received","initial_consult_booked","initial_consult_completed",
            "ct_sim_booked","ready_for_ct_sim","ct_sim_completed",
            "ready_for_initial_contour","ready_for_md_contour",
            "ready_for_dose_calculation","prescription_approved_by_md",
            "ready_for_physics_qa","ready_for_treatment", "patient_arrives",
            "machine_rooms_booked","patient_contacted","patient_scheduled",
            "patient_arrived","treatment_began","end"]


weedComputationList :: (Config Annotation)->[(Computation Annotation)]->Either LexError String
weedComputationList config comps = 
    do

        let compSymbolTable = [emptyScope]
        case (weedFold config compSymbolTable comps) of
            Right r -> Right  r--(trace ("OH" ++ (show r)) r )
            Left e -> Left e

weedFold :: (Config Annotation)->CompSymTable -> [(Computation Annotation)] -> Either LexError String
weedFold conf symtable computations =  
    let errorOrSym = (foldl' (weedEach conf) (Right(symtable,"")) computations)
    in case errorOrSym of
            Right (newSymtable,internalSymRep) ->
                Right $ (internalSymRep++(stringOfLastScope newSymtable))
            (Left e) -> Left e



stringOfLastScope :: CompSymTable -> String
stringOfLastScope symtable =
    let
        len = show $ length symtable
        curr= last symtable
    in HashMap.foldrWithKey  (\(Var s _) t p -> p ++ s ++ "\t" ++
        (tail $ show t) ++"\t" ++len++ "\n" )  "" curr


weedEach::(Config Annotation)->(Either LexError (CompSymTable,String))->(Computation Annotation)->(Either LexError (CompSymTable,String))
weedEach conf errorOrSym comp =  case errorOrSym of
    Right (sym,ret) ->
        case (weedAndTypeCheckComp conf sym comp) of
            Left x ->  Left x --trace ("Error! "++ (show x) ++ " occured but not handled") sym
            Right (newsym,newret) -> Right (newsym,ret++newret)
    _ -> errorOrSym

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
isNowInTopScope  symtable  = (null $ tail symtable)

--evaluateInTopScope :: CompSymTable
evaluateInTopScope symtable f = if isNowInTopScope symtable
    then f symtable
    else Left $ ComputationWrongScope "Can only be in top scope"

isInLoopableScope = error


emptyScope :: HashMap.HashMap (Var Annotation) ComputationType
emptyScope = HashMap.fromList []

weedAndTypeCheckComp :: (Config Annotation) ->(CompSymTable) -> (Computation Annotation)-> Either LexError (CompSymTable, String)
weedAndTypeCheckComp conf symtable  (Table variable constructor  field) =
    evaluateInTopScope symtable check
    where check sym = if ((subFieldExists conf constructor field))
            then Right $ ((addToSymTable sym  variable TTable),"") --(TFilter constructor)
            else Left . FieldNameError $ "Field "++field++
                " does not belong to " ++ constructor
weedAndTypeCheckComp conf symtable (List variable seqlist) =
    evaluateInTopScope symtable check
    where check sym =
            let errorOrSym = foldl' foldWeedList
                    (Right $ addToSymTable sym  variable TList) seqlist
            in case errorOrSym of
                Right s -> Right (s,"")
                Left l -> Left l

weedAndTypeCheckComp conf symtable (Barchart variable) =
    evaluateInTopScope symtable check
    where check sym =
            case getFromSymbolTable sym variable of
                Nothing -> Left . UndefinedVariable $ show  variable
                Just TTable ->  Right (sym,"")
                Just t -> Left . ComputationTypeMismatch $
                            "Cannot draw Barchart of "++ (show variable)
                            ++". It is a " ++ (show t) ++ "Not a Table"

weedAndTypeCheckComp conf symtable (Print printAction ) =
    weedPrintAction symtable printAction
weedAndTypeCheckComp conf symtable (Foreach def comps ) =
    weedForEach conf symtable comps def
weedAndTypeCheckComp conf symtable (Print printAction ) = weedPrintAction  symtable printAction
weedAndTypeCheckComp conf symtable (Foreach def comps ) = weedForEach conf symtable comps def

weedPrintAction :: CompSymTable -> (PrintAction Annotation)-> Either LexError (CompSymTable, String)
weedPrintAction symtable (PrintVar var) = case getFromSymbolTable symtable var of
            Nothing -> Left . UndefinedVariable $ show  var
            Just t -> Right (symtable, "")
weedPrintAction symtable (PrintLength variable) = case getFromSymbolTable symtable variable of
            Nothing -> Left . UndefinedVariable $ show  variable
            Just t -> if t == TTable
                then Right (symtable, "")
                else Left . ComputationTypeMismatch $
                    "Cannot have length of "++ (show variable)++". It is a " ++ (show t) ++ "Not a Table"
weedPrintAction symtable printAction = Left $ ComputationWrongScope "Unimplemented"

weedForEach :: (Config Annotation)->CompSymTable -> [(Computation Annotation)] ->(ForEachDef Annotation) 
    -> Either LexError (CompSymTable,String)
weedForEach conf symtable newcomp (ForEachFilter filterName var )  =
    if (fieldExists conf filterName)
    then    if (isValidInNested conf symtable filterName)
            then let
                    newsym = ( addToSymTable (symtable++[emptyScope])
                                    var (TFilter filterName) )
                in case (weedFold conf newsym newcomp) of
                            Right (intSymRep) -> Right (symtable,intSymRep)
                            Left e -> Left e
            else Left (

                ComputationWrongScope "Foreach is not valid in this scope")
    else Left $  FieldNameError $ "\""++
            filterName++"\" is not a valid loopable Filter"

weedForEach config symtable newcomp (ForEachTable indexVar tableVar)  =
    evaluateInTopScope symtable (\sym->
        case getFromSymbolTable sym tableVar of
            Nothing -> Left . UndefinedVariable $ show  tableVar
            Just TTable -> let
                            newsym = (addToSymTable
                                (sym++[emptyScope]) indexVar TIndex)
                        in case (weedFold config newsym newcomp) of
                            Right (intSymRep) -> Right (sym,intSymRep)
                            Left e -> Left e
            Just t-> Left ( ComputationTypeMismatch $

                    "Cannot go through loop for "++ (show tableVar)
                    ++". It is a " ++ (show t) ++ "Not a Table")
                )
weedForEach config symtable newcomp (ForEachSequence memberVar undefSequence) =
    evaluateInTopScope symtable check
    where check sym=
            let
                newsym = (addToSymTable (sym++[emptyScope])
                                memberVar TSequence)
                errorOrSym = foldl' foldWeedList (Right newsym) undefSequence
            in case errorOrSym of
                Right s -> case (weedFold config s newcomp) of
                    Right (intSymRep) -> Right (s,intSymRep)
                    Left e -> Left e
                Left l -> Left l

weedForEach config symtable newcomp (ForEachList memberVar listVar) =

    evaluateInTopScope symtable check
    where check sym=
            case getFromSymbolTable sym listVar of
                Nothing -> Left . UndefinedVariable $ show  listVar
                Just TList -> let
                                newsym =(addToSymTable (sym++[emptyScope])
                                    memberVar TSequence)
                        in case (weedFold config newsym newcomp) of
                            Right (intSymRep) -> Right (sym,intSymRep)
                            Left e -> Left e
                Just t-> Left ( ComputationTypeMismatch $
                        "CAnnot Go through loop for "++ (show listVar)++
                        ". It is a " ++ (show t) ++ "Not a List")


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

isValidInNested ::(Config Annotation)-> CompSymTable -> FilterName -> Bool
isValidInNested conf symtable filterName =
    let
        (counts, filtersUsed) = unzip (findAllFilters symtable)
    in ((null filtersUsed) || ( not (elem filterName filtersUsed)) && ((head counts)==1) )


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
