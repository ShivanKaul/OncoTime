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
import Debug.Trace
--Our modules
import TypeUtils
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
            Right weededGrpFiles -> putStrLn $ file ++ ": All Group files exist"

        --parsing each group file
        let grpAllFilesContents = map (readFile) (useFilesToParse)
        newGroups <- sequence (map (getGroupDefs) (grpAllFilesContents))

        let allGroups = (concat (newGroups)) ++ groupDefs
        --may need a function to annotate everything
        print allGroups
        putStrLn "\n"
        -- check for redeclarations for group
        case (checkForGroupRedecl allGroups) of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> print r >> print  "No redeclarations for group files"

        -- take care of recursive groups
        case (checkForRecursiveGroups allGroups) of
            [] -> print "No recursively defined groups"
            vars -> hPrint stderr (RecursiveGroups ("Following group vars " ++
                "recursively defined: " ++ varsToStr (vars))) >> exitFailure
        -- Check types of groups
        print allGroups
        -- get list of all types of groups
        let groupTypesList = map (\(Group gtype _ _) -> gtype) allGroups
        print groupTypesList
        -- get list of all valid types
        let validTypes = [GroupType "Sex", GroupType "id", GroupType "birthyear", GroupType "diagnosis", GroupType "gender", GroupType "postalcode", GroupType "years", GroupType "days", GroupType "months", GroupType "oncologist"]
        case (checkIfValidGroupTypes groupTypesList validTypes) of
            True -> print "Group types valid!"
            False -> hPrint stderr "Group Type invalid!" >> exitFailure
        -- check if variable being used in group is actually defined in symbol table #83
        -- check types of groups and if they exist #99
        let symbolTableH = buildHeadSymbolTable allGroups hdr

        --check erroneous subfields i.e. whether all fields exist
        case (checkFilters filters filterable) of
            Left e -> print e >>  hPutStrLn stderr "FILTERS:" >>
                print filters >> putStrLn "CONF:" >> print filterable >> exitFailure
            Right checkedFilters -> putStrLn "All Fields valid"

        print "test"

        case  mapM (checkFilterTypes conf symbolTableH) filters of
                Left e -> hPrint stderr e >> print [filters] >> exitFailure
                Right annotatedFilters -> do
                    print filters

                    print "Printing old filters"

                    print filters

                    print "Printing new filters"

                    --let newFilters = (populateDefaultValuesFilters filters (Config confmap))

                    let newFilters = (populateDefaultValuesFilters (annotatedFilters) (Config confmap))
                    print newFilters



                    -- Replace vars with symbol table h
                    print "Printing with vars replaced"

                    let filtersWithVarsReplaced = (map (replaceVarsFilter symbolTableH) newFilters)
                    print filtersWithVarsReplaced

                    -------- UNTEST -------

                    case (M.lookup ("patient", True) confmap) of
                        Nothing -> hPrint stderr "key not found in confmap!"
                        Just (FieldMap r) -> print "here's a confmap" >> print (show (M.toList r))

                    let compsResult=   weedComputationList conf comps 
                    case  compsResult of    
                        Left e-> hPrint stderr e >>exitFailure
                        Right (str,ann) -> symTabFun str
                    let annComps = case  compsResult of    
                            Left e-> []
                            Right (str,ann) -> ann

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
                    return (Program hdr docs [] (allGroups) filtersWithVarsReplaced (annComps))


checkIfValidGroupTypes :: [GroupType] -> [GroupType] -> Bool
checkIfValidGroupTypes groupTypes validTypes =
    do
        foldl (\bool grp -> if grp `elem` validTypes then bool else False) True groupTypes

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
  hashWithSalt s t@(Var v a) = s + (hash v)

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
populateDefaultValuesFilters :: [Filter Annotation] -> (Config Annotation)-> [Filter Annotation]
populateDefaultValuesFilters filters config =
    do
        map (findDefaultValuesFilt config) (filters)

findDefaultValuesFilt :: (Config Annotation)-> Filter Annotation -> Filter Annotation
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

fieldMapToFieldDefs :: (FieldMap Annotation) -> [FieldDef Annotation]
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
checkFilterTypes::(Config Annotation)->(HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]))->(Filter Annotation)->Either LexError (Filter Annotation) -- ()--[(Filter Annotation)]
checkFilterTypes (Config conf) hmap ms =
    do
--How can I accumulate stuff?
       -- forM_ ms  (\x -> do
            --from fields
            
            let filterName = (getFilterName ms)
            let fieldDefs = getFieldDefList ms -- list of possible FieldDefs for a filter
            --from confmap
            
            case (M.lookup (filterName, True) conf) of--things to check against for that filter
                Just val ->  do
                    trace ("calling f with x = ") (pure( )) 
                    case ((typeCheckFieldMap val hmap fieldDefs)) of
                        Left e -> Left e
                        Right r ->  Right $ (Filter filterName r)
                    --trace ("calling f with x = ") (pure( )) 

                Nothing -> do
                    case (M.lookup (filterName, False) conf) of
                        Nothing -> Left $ GenError (filterName ++ " is not in map")
                        Just val -> do

                            case ((typeCheckFieldMap val hmap fieldDefs)) of
                                Left e -> Left e
                                Right r -> Right $ (Filter filterName r )

--return a list of things that don't type check
--NEED TO RETURN ANNOTATED FIELD MAP MAYBE?
typeCheckFieldMap::(FieldMap Annotation) -> (HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]))-> [FieldDef Annotation] ->Either LexError [(FieldDef Annotation)]
typeCheckFieldMap (FieldMap fm) hmap fdList = do
   
   foldM (\acc x-> do 
            let fieldName = getFieldName x
            let fvalList  = getFieldValList x
            case (M.lookup fieldName fm) of
                Nothing -> do
                    
                    trace ("calling f with x =  ") (pure( )) 
                    Left $ GenError "Not somethign"
                Just val -> 
                    do
                        case (mapM (compareFieldTypes val (FieldMap fm) hmap) fvalList) of
                            Left e -> do
                                
                                Left e
                            Right r -> do 
                                
                                Right $ (FieldDef fieldName r):acc
    ) [] fdList


--take fieldDefs anda  fieldMap, return an error or a field deaf after calling Field
compareFieldTypes::(Field Annotation)->(FieldMap Annotation)->(HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]))->(GroupItem Annotation)->Either LexError (GroupItem Annotation)
compareFieldTypes (FieldValue allValList an ) fm hm (GroupValString s a)  = 
    if (s `elem` allValList) 
        then Right (GroupValString s an) --Right (GroupValString s) 
    else Left (AllowedValError ("Error. " ++ s ++ " is not defined in the config file list that also contains: " ++ (intercalate "," allValList)))
--compareFieldTypes (FieldType "String" an) fm hm (GroupValString s a)  = Right (GroupValString s (an))
--compareFieldTypes (FieldType "Int" an) fm hm (GroupRange a ) = Right (GroupRange a)
compareFieldTypes (FieldType "Date" an) fm hm (GroupDate a b c _) = Right (GroupDate a b c (an))
compareFieldTypes (FieldType t (Annotation an)) fm hm (GroupVar v@(Var var (Annotation a))) = if an == a then Right (GroupVar v) else Left $ TypeError ("Error. Type mismatch. Field of type " ++ show t ++ "Var " ++ show var ++ " of type " ++ show a)
compareFieldTypes (FieldVar a an) fm hm (GroupVar (Var v _)) = Right (GroupVar (Var v an))
--I need a new type that allows me to pull out the type of var
compareFieldTypes f (FieldMap fm) hm  (GroupVar var@(Var v an) ) =
    do
        --Left $ TypeError ("ERROR. var " ++ (varToStr var) ++ " not declared " ++ "FIELD IS: " ++ show f ++ " VAR IS : " ++ show var ++ "\n" ++ show fm )
        --trace ("calling f with x =  ") (pure( )) 
        case (HashMap.lookup var hm) of
            Nothing -> Left $ TypeError ("ERROR. var " ++ (varToStr var) ++ " not declared " ++ "FIELD IS: " ++ show f ++ " VAR IS : " ++ show var ++ "\n" ++ show hm ++ "\n")
            
            Just (a,_) -> case (M.member ((groupTypeToStr a)) fm) of
            --if it is declared, we want to make sure it is used in the right case. i.e. with the right field
                False -> Left $ TypeError ("ERROR. variable " ++
                        (varToStr var) ++" is used with wrong field")
                True  -> Right (GroupVar (Var v an))

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


weedComputationList :: (Config Annotation)->[(Computation Annotation)]->Either LexError (String,[(Computation Annotation)])
weedComputationList config comps =
    do

        let compSymbolTable = [emptyScope]
        case (weedFold config compSymbolTable comps) of
            Right r -> Right  r--(trace ("OH" ++ (show r)) r )
            Left e -> Left e

weedFold :: (Config Annotation)->CompSymTable -> [(Computation Annotation)] -> Either LexError (String,[(Computation Annotation)])
weedFold conf symtable computations =
    let errorOrSym = (foldl' (weedEach conf) (Right(symtable,"",[])) computations)
    in case errorOrSym of
            Right (newSymtable,internalSymRep,newcops) ->
                Right $ (internalSymRep++(stringOfLastScope newSymtable),newcops)
            (Left e) -> Left e



stringOfLastScope :: CompSymTable -> String
stringOfLastScope symtable =
    let
        len = show $ length symtable
        curr= last symtable
    in HashMap.foldrWithKey  (\(Var s _) t p -> p ++ s ++ "\t" ++
        (tail $ show t) ++"\t" ++len++ "\n" )  "" curr


weedEach::(Config Annotation)->(Either LexError (CompSymTable,String,[(Computation Annotation)]))
    ->(Computation Annotation)->(Either LexError (CompSymTable,String,[(Computation Annotation)]))
weedEach conf errorOrSym comp =  case errorOrSym of
    Right (sym,ret,compsSoFar) ->
        case (weedAndTypeCheckComp conf sym comp) of
            Left x ->  Left x --trace ("Error! "++ (show x) ++ " occured but not handled") sym
            Right (newsym,newret,newAnnCom) -> Right (newsym,ret++newret,compsSoFar++[newAnnCom])
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

weedAndTypeCheckComp :: (Config Annotation) ->(CompSymTable) -> (Computation Annotation)
    -> Either LexError (CompSymTable, String,(Computation Annotation))
weedAndTypeCheckComp conf symtable  (Table variable@(Var name _) constructor  field) =
    evaluateInTopScope symtable check
    where check sym = if ((subFieldExists conf constructor field))
            then Right $ ((addToSymTable sym  variable TTable),"", Table (Var name (Annotation "Table")) constructor  field) --(TFilter constructor)
            else Left . FieldNameError $ "Field "++field++
                " does not belong to " ++ constructor
weedAndTypeCheckComp conf symtable (List variable@(Var name _) seqlist) =
    evaluateInTopScope symtable check
    where check sym =
            let errorOrSym = foldl' foldWeedList
                    (Right $ addToSymTable sym  variable TList) seqlist
            in case errorOrSym of
                Right s -> Right (s,"", List (Var name (Annotation "List")) seqlist)
                Left l -> Left l

weedAndTypeCheckComp conf symtable (Barchart variable@(Var name _)) =
    evaluateInTopScope symtable check
    where check sym =
            case getFromSymbolTable sym variable of
                Nothing -> Left . UndefinedVariable $ prettyPrint  variable
                Just TTable ->  Right (sym,"",Barchart (Var name (Annotation $ "Table")))
                Just t -> Left . ComputationTypeMismatch $
                            "Cannot draw Barchart of "++ (show variable)
                            ++". It is a " ++ (prettyPrint t) ++ "Not a Table"

weedAndTypeCheckComp conf symtable (Print printAction ) = 
    weedPrintAction conf symtable printAction
weedAndTypeCheckComp conf symtable (Foreach def comps ) = 
    weedForEach conf symtable comps def

weedPrintAction :: (Config Annotation)-> CompSymTable -> (PrintAction Annotation)
            -> Either LexError (CompSymTable, String,(Computation Annotation))
weedPrintAction _ symtable  (PrintVar var@(Var name _)) = 
    case getFromSymbolTable symtable var of
            Nothing -> Left . UndefinedVariable $ prettyPrint  name 
            Just t -> Right (symtable, "",
                Print $ PrintVar $ Var name $ Annotation $ prettyPrint t)

weedPrintAction _ symtable (PrintLength var@(Var name _)) = 
    case getFromSymbolTable symtable var of
            Nothing -> Left . UndefinedVariable $ prettyPrint  var 
            Just TTable ->  Right (symtable, "",
                    Print $ PrintLength $ Var name $ Annotation $ prettyPrint TTable)
            Just wrong ->  Left . ComputationTypeMismatch $ 
                    "Cannot have length of "++ (prettyPrint var)++
                    ". It is a " ++ (prettyPrint wrong) ++ "Not a Table"

weedPrintAction _ symtable (PrintElement tableVar@(Var tname _) indexVar@(Var iname _) ) = 
    case ((getFromSymbolTable symtable tableVar),
        (getFromSymbolTable symtable indexVar)) of
            (Nothing,_) -> Left . UndefinedVariable $ prettyPrint  tableVar
            (_,Nothing) -> Left . UndefinedVariable $ prettyPrint  indexVar
            (Just TTable, Just TIndex) ->  Right (symtable,"",
                    Print ( PrintElement (Var tname $ Annotation $ prettyPrint TTable)
                        ( (Var iname $ Annotation $ prettyPrint TIndex) ) ) )
            (Just TTable, Just i)-> Left . ComputationTypeMismatch $
                        "Cannot access "++ (prettyPrint indexVar)++" of table "
                        ++(prettyPrint tableVar)
                        ++". It is a " ++ (prettyPrint i) ++ "Not an Index"
            (Just t,Just i) -> Left . ComputationTypeMismatch $
                    "Cannot index "++ (prettyPrint tableVar)++". It is a " 
                    ++ (prettyPrint t) ++ "Not a Table"

weedPrintAction config symtable (PrintFilters fields filterVar@(Var fname _) ) = 
    case (getFromSymbolTable symtable filterVar) of
            (Nothing) -> Left . UndefinedVariable $ prettyPrint filterVar
            (Just (TFilter constructor)) -> 
                if all (\s -> subFieldExists config constructor s) fields 
                then Right (symtable,"",
                    Print (PrintFilters fields (Var fname $ Annotation $ constructor)))
                else Left . FieldNameError $ "One of the fields "++
                            (show fields)++
                           " does not belong to " ++ constructor
            Just wrong -> Left . ComputationTypeMismatch $
                    "Cannot filter over "++ (prettyPrint filterVar)++
                    ". It is a " ++ (prettyPrint wrong) ++ " Not a Filter"

weedPrintAction _ symtable (PrintTimeLine filterVar@(Var fname _)) = 
    case (getFromSymbolTable symtable filterVar) of
            (Nothing) -> Left . UndefinedVariable $ show  filterVar
            (Just (TFilter "patient")) -> Right (symtable,"", 
                Print (PrintTimeLine  (Var fname $ Annotation $ "patients")))
            (Just (TFilter "patients")) -> Right (symtable,"",
                Print (PrintTimeLine  (Var fname $ Annotation $ "patients")))
            Just wrong -> Left . ComputationTypeMismatch $
                    "Cannot print TimeLine of "++ (prettyPrint filterVar)++
                    ". It is a " ++ (prettyPrint wrong) ++ " Not a patient"

weedForEach :: (Config Annotation)->CompSymTable -> [(Computation Annotation)] ->(ForEachDef Annotation)
    -> Either LexError (CompSymTable,String,(Computation Annotation))
weedForEach conf symtable newcomp (ForEachFilter filterName var@(Var iname _) )  =
    if (fieldExists conf filterName)
    then    if (isValidInNested conf symtable filterName)
            then let
                    newsym = ( addToSymTable (symtable++[emptyScope])
                                    var (TFilter filterName) )
                in case (weedFold conf newsym newcomp) of
                            Right (intSymRep,annComps) -> Right (symtable,intSymRep, 
                                (Foreach (ForEachFilter filterName (Var
                                 iname (Annotation filterName)) ) annComps) )
                            Left e -> Left e
            else Left (
                ComputationWrongScope "Foreach is not valid in this scope")
    else Left $  FieldNameError $ ""++
            filterName++" is not a valid loopable Filter"

weedForEach config symtable newcomp (ForEachTable indexVar@(Var iname _) tableVar@(Var tname _) )  =
    evaluateInTopScope symtable (\sym->
        case getFromSymbolTable sym tableVar of
            Nothing -> Left . UndefinedVariable $ prettyPrint  tableVar
            Just TTable -> let
                            newsym = (addToSymTable
                                (sym++[emptyScope]) indexVar TIndex)
                        in case (weedFold config newsym newcomp) of
                            Right (intSymRep,annComps) -> Right (sym,intSymRep, (Foreach (ForEachTable (Var iname $Annotation"index") (Var tname $Annotation"Table")) annComps) )
                            Left e -> Left e
            Just t-> Left ( ComputationTypeMismatch $
                    "Cannot go through loop for "++ (prettyPrint tableVar)
                    ++". It is a " ++ (prettyPrint t) ++ "Not a Table")
                )
weedForEach config symtable newcomp (ForEachSequence memberVar@(Var mname _) undefSequence) =
    evaluateInTopScope symtable check
    where check sym=
            let
                newsym = (addToSymTable (sym++[emptyScope])
                                memberVar TSequence)
                errorOrSym = foldl' foldWeedList (Right newsym) undefSequence
            in case errorOrSym of
                Right s -> case (weedFold config s newcomp) of
                    Right (intSymRep,annComps) -> Right (s,intSymRep, (Foreach  ( (ForEachSequence (Var mname (Annotation "sequence member")) undefSequence) ) annComps))
                    Left e -> Left e
                Left l -> Left l

weedForEach config symtable newcomp (ForEachList memberVar@(Var mname _) listVar@(Var lname _)) =

    evaluateInTopScope symtable check
    where check sym=
            case getFromSymbolTable sym listVar of
                Nothing -> Left . UndefinedVariable $ prettyPrint  listVar
                Just TList -> let
                                newsym =(addToSymTable (sym++[emptyScope])
                                    memberVar TSequence)
                        in case (weedFold config newsym newcomp) of
                            Right (intSymRep,annComps) -> Right (sym,intSymRep,Foreach (ForEachList (Var mname  (Annotation"sequence member")) (Var lname (Annotation"List"))) annComps)
                            Left e -> Left e
                Just t-> Left ( ComputationTypeMismatch $
                        "CAnnot Go through loop for "++ (prettyPrint listVar)++
                        ". It is a " ++ (prettyPrint t) ++ "Not a List")


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
        filtersUsed = (findAllFilters symtable)
        noFilters = null filtersUsed
        topScope = isNowInTopScope symtable
        prevUsed = (elem filterName filtersUsed)
    in ((noFilters && topScope)|| ( (not noFilters)&&(not prevUsed)))


findAllFilters :: CompSymTable -> [FilterName]
findAllFilters symtable = foldl' (\ p  (s)-> let f =find1Filter s
                                        in if (not $ null f)
                                            then p++[(f)]
                                            else p) [] (symtable)
find1Filter :: Scope -> FilterName
find1Filter curr = HashMap.foldr  (\ t p -> if (null p)
                                    then case t of
                                        TFilter val -> val
                                        _ -> p
                                    else p)  "" curr
