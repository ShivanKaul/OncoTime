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
        let symbolTableH = buildHeadSymbolTable allGroups hdr


        case  mapM_ (checkFilterTypes (filterable) symbolTableH) [filters] of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> putStrLn "all field types check out"



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



        -- print (show (prg))

        putStrLn "Weeded successfully"
        return (Program hdr docs [] (allGroups) filters (comps))

-- Make Var hashable
instance (Hashable Var) where
  hashWithSalt s (Var v) = s + (hash v)

-- Utility test function to check if symbol table contains a key
testIfHSymbolTableContains :: HashMap.HashMap Var GroupType -> Var -> IO()
testIfHSymbolTableContains hashmap (Var v) =
        case (HashMap.lookup (Var v) hashmap) of
            Nothing -> hPrint stderr "nothing found!"
            Just r -> print ("Found VALUE " ++ show r ++ " for KEY " ++
                v ++ " in symboltable1")

-- Build symbol table from groups
buildHeadSymbolTable :: [GroupDefs] -> Header -> HashMap.HashMap Var GroupType
buildHeadSymbolTable groups (Header _ args) =
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
                    False -> Left $ MissingFilesError (
                        "ERROR: Missing one of group files: " 
                        ++ ( intercalate ","  declaredUseFiles) ++ 
                        " out of: " ++ (intercalate "," grpFiles)) 
                    --Better error messages for other cases. Maybe see what files
                    -- are missing exactly. Doesn't need to be true false exactly
                    True -> Right $ useList


--what is the symbol table doing exactly?
--buildBodySymbolTable::[Computation]->M.Map Var  Type??
--Table
--List

getGroupDefs::IO(String) -> IO([GroupDefs])
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
            False -> Left $ MissingFilesError "ERROR: Group files Missing" 
            --Better error messages for other cases. Maybe see what files are missing exactly. 
            --Doesn't need to be true false exactly
            True -> Right $ useFiles
checkFilters::[Filter]->Config->Either LexError [Filter]
checkFilters filList conf = case (checkFilRedec filList ) of
    Right r ->
        case (checkFieldsEx conf r [] ) of
            [] -> Right filList
            l -> Left $ MissingConfigField $ "Error. Fields Missing in " ++ 
                    (M.showTreeWith (\k x -> show (k,x)) True False (M.fromList l) )
    Left e -> Left e
--Highest level, checkFilters. Is in the either monad to give us error checking

getFilterName::Filter->FilterName
getFilterName (Filter f _) = f

checkFilRedec::[Filter]->Either LexError [Filter]
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
checkFiltConf::[Filter]->Config->Either LexError [Filter]
checkFiltConf x conf =
    case (checkFields conf x []) of
        [] -> Right x
        l -> Left $ MissingConfigField (
            "The following fields are not specified in the config file" 
            ++ (intercalate ", " (map (getFilterName) l)) )

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
fieldExists (Config confmap) fname = (M.member (fname, True) confmap) 
                                        || (M.member (fname, False) confmap)

subFieldExists::Config->FilterName->FieldName->Bool
subFieldExists (Config confmap) fname sfname =
    case (M.lookup (fname, True) confmap) of
        Nothing -> case (M.lookup (fname, False) confmap) of
            Nothing-> False
            Just (FieldMap m) -> M.member sfname m
        Just (FieldMap m) -> M.member sfname m

--given a list of filters, makes sure each thing int he map belongs
--COULD ALSO DO TYPE CHEKING HERE, GIVEN THE SYMBOL TABLE
checkFieldsEx::Config->[Filter]->[(FilterName, [FilterName])]
    ->[(FilterName, [FilterName])]
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
checkFilterTypes::Config->(HashMap.HashMap Var GroupType)->[Filter]->Either LexError ()--[Filter]
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
typeCheckFieldMap::FieldMap->(HashMap.HashMap Var GroupType)->[FieldDef]->Either LexError () --[FieldDef]
typeCheckFieldMap (FieldMap fm) hmap fdList = do
   forM_ fdList $ \x ->
        do
            let fieldName = getFieldName x
            let fvalList  = getFieldValList x
            case (M.lookup fieldName fm) of
                Nothing -> Left $ GenError "Not somethign"
                Just val -> mapM_ (compareFieldTypes val (FieldMap fm) hmap) fvalList

--take fieldDefs anda  fieldMap, return an error or a field deaf after calling Field
compareFieldTypes::Field->FieldMap->(HashMap.HashMap Var GroupType)
    ->GroupItem->Either LexError ()--GroupItem
compareFieldTypes (FieldVal allValList) fm hm (GroupValString s)  =
    if (s `elem` allValList) then Right ()--Right (GroupValString s)
    else Left (AllowedValError ("Error. " ++ s ++ 
        " is not defined in the config file list that also contains: " 
        ++ (intercalate "," allValList)))
compareFieldTypes (FieldType "String") fm hm (GroupValString _)  = Right ()
compareFieldTypes (FieldType "Int") fm hm (GroupRange _) = Right ()
compareFieldTypes (FieldType "Date") fm hm (GroupDate _ _ _) = Right ()
--I need a new type that allows me to pull out the type of var
compareFieldTypes _ (FieldMap fm) hm  (GroupVar var) =
    case (HashMap.lookup var hm) of
        Nothing -> Left $ TypeError ("ERROR. var " ++ (varToStr var) ++ "not declared")
        Just a -> case (M.member ((groupTypeToStr a)) fm) of 
        --if it is declared, we want to make sure it is used in the right case. i.e. with the right field
                False -> Left $ TypeError ("ERROR. variable " ++ 
                    (varToStr var) ++" is used with wrong field")
                True  -> Right ()
compareFieldTypes b fm hm a = Left $ TypeError ("Type Error between " ++ 
    (show a) ++ " and " ++ (show b))

varToStr::Var->String
varToStr (Var v) = v

groupTypeToStr::GroupType->String
groupTypeToStr (GroupType s) = s


events :: [String]
events = ["consult_referral_received","initial_consult_booked","initial_consult_completed",
            "ct_sim_booked","ready_for_ct_sim","ct_sim_completed",
            "ready_for_initial_contour","ready_for_md_contour",
            "ready_for_dose_calculation","prescription_approved_by_md",
            "ready_for_physics_qa","ready_for_treatment", "patient_arrives",
            "machine_rooms_booked","patient_contacted","patient_scheduled","patient_arrived","treatment_began","end"]


weedComputationList :: Config->[Computation]->Either LexError String
weedComputationList config comps = 
        let compSymbolTable = [emptyScope]
        in case (weedFold config compSymbolTable comps) of
            Right r -> Right  r--(trace ("OH" ++ (show r)) r )
            Left e -> Left e 

weedFold :: Config->CompSymTable -> [Computation] -> Either LexError String
weedFold conf symtable computations = 
    let errorOrSym = (foldl' (weedEach conf) (Right(symtable,"")) computations)
    in case errorOrSym of
            Right (newSymtable,internalSymRep) -> 
                Right $ (internalSymRep++(stringOfLastScope newSymtable))
            (Left e) -> Left e
        --res <-  printFold (foldl' (weedEach conf) symtable computations)
        --return res
            --Left e -> Left e
            --Right r -> Right r

weedEach::Config->(Either LexError (CompSymTable,String))->Computation
    ->(Either LexError (CompSymTable,String))
weedEach conf errorOrSym comp =  case errorOrSym of
    Right (sym,ret) ->
        case (weedAndTypeCheckComp conf sym comp) of
            Left x ->  Left x --trace ("Error! "++ (show x) ++ " occured but not handled") sym
            Right (newsym,newret) -> Right (newsym,ret++newret)
    _ -> errorOrSym

stringOfLastScope :: CompSymTable -> String
stringOfLastScope symtable =
    let
        len = show $ length symtable
        curr= last symtable
    in HashMap.foldrWithKey  (\(Var s) t p -> p ++ s ++ "\t" ++ 
        (tail $ show t) ++"\t" ++len++ "\n" )  "" curr



addToSymTable :: CompSymTable -> Var -> ComputationType-> CompSymTable
addToSymTable symtable v  comptype =
    let prev= init symtable
        local = last symtable
        updated = HashMap.insert  v comptype local
    in prev++[updated]

type Scope = HashMap.HashMap Var ComputationType

type CompSymTable = [Scope]

getFromScope:: Scope ->Var-> Maybe ComputationType
getFromScope  hashmap v = (HashMap.lookup v hashmap)

getFromSymbolTable :: CompSymTable -> Var -> Maybe ComputationType
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


emptyScope :: HashMap.HashMap Var ComputationType
emptyScope = HashMap.fromList []


weedAndTypeCheckComp :: Config->CompSymTable -> Computation 
    -> Either LexError (CompSymTable,String)
weedAndTypeCheckComp conf symtable  (Table variable constructor  field) =
    evaluateInTopScope symtable check
    where check sym = if ((subFieldExists conf constructor field))
            then Right $ ((addToSymTable sym  variable TTable),"") --(TFilter constructor)
            else Left . FieldNameError $ "Field "++field++
                " does not belong to " ++ constructor
weedAndTypeCheckComp conf symtable (List variable seqlist) =
    evaluateInTopScope symtable check
    where check sym = 
            let errorOrSym = foldl' foldWeedList (Right $ addToSymTable sym  variable TList) seqlist
            in case errorOrSym of 
                Right sym -> Right (sym,"")
                Left l -> Left l
weedAndTypeCheckComp conf symtable (Barchart variable) =
    evaluateInTopScope symtable check
    where check sym =
            case getFromSymbolTable sym variable of
                Nothing -> Left . UndefinedVariable $ show  variable
                Just TTable ->  Right (symtable,"")
                Just t -> Left . ComputationTypeMismatch $
                            "Cannot draw Barchart of "++ (show variable)
                            ++". It is a " ++ (show t) ++ "Not a Table"

weedAndTypeCheckComp conf symtable (Print printAction) = 
    weedPrintAction conf symtable printAction
weedAndTypeCheckComp conf symtable (Foreach def comps) = 
    weedForEach conf symtable comps def

weedPrintAction :: Config-> CompSymTable -> PrintAction 
    -> Either LexError (CompSymTable,String)
weedPrintAction _ symtable (PrintVar var) = 
    case getFromSymbolTable symtable var of
            Nothing -> Left . UndefinedVariable $ show  var
            Just t -> Right (symtable,"")
weedPrintAction _ symtable (PrintLength variable) = 
    case getFromSymbolTable symtable variable of
            Nothing -> Left . UndefinedVariable $ show  variable
            Just TTable -> Right (symtable,"")
            Just t -> Left . ComputationTypeMismatch $
                    "Cannot have length of "++ (show variable)++
                    ". It is a " ++ (show t) ++ "Not a Table"
weedPrintAction _ symtable (PrintElement tableVar indexVar) = 
    case ((getFromSymbolTable symtable tableVar),
        (getFromSymbolTable symtable indexVar)) of
            (Nothing,_) -> Left . UndefinedVariable $ show  tableVar
            (_,Nothing) -> Left . UndefinedVariable $ show  indexVar
            (Just TTable, Just TIndex) ->  Right (symtable,"")
            (Just TTable, Just i)-> Left . ComputationTypeMismatch $
                        "Cannot access "++ (show indexVar)++" of table "
                        ++(show tableVar)
                        ++". It is a " ++ (show i) ++ "Not an Index"
            (Just t,Just i) -> Left . ComputationTypeMismatch $
                    "Cannot index "++ (show tableVar)++". It is a " 
                    ++ (show t) ++ "Not a Table"
weedPrintAction config symtable (PrintFilters fields filterVar) = 
    case (getFromSymbolTable symtable filterVar) of
            (Nothing) -> Left . UndefinedVariable $ show  filterVar
            (Just (TFilter constructor)) -> 
                if all (\s -> subFieldExists config constructor s) fields 
                then Right (symtable,"")
                else Left . FieldNameError $ "One of the fields "++
                            (show fields)++
                           " does not belong to " ++ constructor
            Just wrong -> Left . ComputationTypeMismatch $
                    "Cannot filter over "++ (show filterVar)++
                    ". It is a " ++ (show wrong) ++ " Not a Filter"

weedPrintAction _ symtable (PrintTimeLine filterVar) = 
    case (getFromSymbolTable symtable filterVar) of
            (Nothing) -> Left . UndefinedVariable $ show  filterVar
            (Just (TFilter "patient")) -> Right (symtable,"")
            (Just (TFilter "patients")) -> Right (symtable,"")
            Just wrong -> Left . ComputationTypeMismatch $
                    "Cannot print TimeLine of "++ (show filterVar)++
                    ". It is a " ++ (show wrong) ++ " Not a patient"

weedForEach :: Config->CompSymTable -> [Computation] ->ForEachDef 
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
                                (symtable++[emptyScope]) indexVar TIndex)
                        in case (weedFold config newsym newcomp) of
                            Right (intSymRep) -> Right (symtable,intSymRep) 
                            Left e -> Left e  
            Just t-> Left ( ComputationTypeMismatch $
                    "Cannot go through loop for "++ (show tableVar)
                    ++". It is a " ++ (show t) ++ "Not a Table")
                )
weedForEach config symtable newcomp (ForEachSequence memberVar unused) = 
    Left $ ComputationWrongScope "Unimplemented"
weedForEach config symtable newcomp (ForEachList memberVar listVar)= 
    evaluateInTopScope symtable check
    where check sym=
            case getFromSymbolTable sym listVar of
                Nothing -> Left . UndefinedVariable $ show  listVar
                Just TList -> let
                                newsym =(addToSymTable (symtable++[emptyScope])
                                 memberVar TSequence)
                        in case (weedFold config newsym newcomp) of
                            Right (intSymRep) -> Right (symtable,intSymRep) 
                            Left e -> Left e 
                Just t-> Left ( ComputationTypeMismatch $
                        "CAnnot Go through loop for "++ (show listVar)++
                        ". It is a " ++ (show t) ++ "Not a List")

weedSequence :: SeqField -> Either String Bool
weedSequence (Bar evlist) = checkEvents evlist
weedSequence (Comma evlist) = checkEvents evlist
weedSequence (Star evlist) = checkEvents evlist
weedSequence (Neg (ev)) = checkEvents [ev]

checkEvents :: [Event] -> Either String Bool
checkEvents evlist = foldl' (\prev (Event curr) ->
        if ( elem curr events)
        then prev
        else Left curr)
    (Right True) evlist
foldWeedList :: (Either LexError CompSymTable) 
    -> SeqField -> (Either LexError CompSymTable)
foldWeedList prev curr =
    case weedSequence curr of
        Left evname -> Left $ IncorrectEvent evname
        _-> prev


isValidInNested ::Config-> CompSymTable -> FilterName -> Bool
isValidInNested conf symtable filterName =
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
