{-
OncoTime - Implementation of cancer-research domain-specific language as a project undertaken for
COMP 520 - Compiler Design in Winter 2016 at McGill University by

Shivan Kaul Sahib
Yusaira Khan
Brendan Games Gordon

The course was taught by Laurie Hendren.
 -}

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
-- import qualified Text.Parsec.Pos as Pos
import Data.Char
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Pos
import Debug.Trace
--Our modules
import TypeUtils
import Types
import Parser
import PrettyPrinter
import Formatter

--the function that does all weeding
weed::String->(String -> IO())->(Program Annotation)->SourcePos->IO((Program Annotation),(Config Annotation))
weed file symTabFun prg@(Program hdr@(Header _ paramList)  docs useList groupDefs filters comps) pos =
    do
        let dummySrcPos = (newPos "" 1 1)
        putStrLn $ "File "++file
        let validTypes = [GroupType "sex" dummySrcPos, GroupType "id" dummySrcPos, GroupType "birthyear" dummySrcPos,
                GroupType "diagnosis" dummySrcPos, GroupType "gender" dummySrcPos,
                GroupType "postalcode" dummySrcPos, GroupType "years" dummySrcPos, GroupType "date" dummySrcPos,
                GroupType "days" dummySrcPos, GroupType "months" dummySrcPos, GroupType "oncologist" dummySrcPos, GroupType "events" dummySrcPos]
        -- get params from header
        let headerParams = case hdr of
                (Header fname args) -> args
        -- get types of params
        let paramTypesList = map (\(Arg gtype _) -> gtype) headerParams
        -- Print out line number where error encountered
        -- Print out TypeError
        case (checkIfValidGroupTypes paramTypesList validTypes) of
            Nothing -> putStrLn $ "Header params have valid types!"
            Just grp@(GroupType name srcpos) -> hPrint stderr (InvalidTypesParams
                ("Header params have invalid types: " ++ (show name) ++ " on " ++ (show srcpos))) >> exitFailure

        --get Config file
        conf <- readConfig file

       --grpFile weeding
        case dupesExist (map (\x -> (Var x (Annotation "") (newPos "" 1 1))) (flattenUseFile useList)) of
            (_, []) -> putStrLn $ "Uselist is fine!"
            (_, repeated) -> hPrint stderr (RedecError ("The following group files were redeclared: "
                ++ (intercalate ", " (map (varToStr) repeated)))) >> exitFailure
        curContents <- (getDirectoryContents  $ dropFileName file)
        let dirContents = curContents -- ++ valContents ++ invContents
        let grpFiles = filter (\x -> takeExtension x == ".grp") dirContents
        let grpFileNames = map dropExtension grpFiles
        let grpFileList = weedGroupFiles useList grpFileNames
        let useFilesToParse = map (\x -> (dropFileName file) ++ x ++ ".grp") (flattenUseFile useList)

        let (Config confmap) = conf
            loopable = Config $ M.filterWithKey (\(name,valid) t -> valid) confmap
            filterable = Config $ M.filterWithKey (\(name,valid) t -> not valid) confmap

        case grpFileList of
            Left e -> hPutStrLn stderr (file ++ ": ") >> hPrint stderr e >> exitFailure
            Right weededGrpFiles -> putStrLn $ file ++ ": All Group files exist"

        --parsing each group file
        let grpAllFilesContents = map (readFile) (useFilesToParse)
        newGroups <- sequence (map (getGroupDefs) (grpAllFilesContents))

        -- check for Diagnosis -> Patient -> Doctor

        let allGroups = (concat (newGroups)) ++ groupDefs
        --may need a function to annotate everything

        -- check for redeclarations for group
        case (checkForGroupRedecl allGroups) of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> (putStrLn "No redeclarations for group files!")

        -- take care of recursive groups
        case (checkForRecursiveGroups allGroups) of
            [] -> putStrLn $ "No recursively defined groups!"
            vars -> hPrint stderr (RecursiveGroups ("Following group vars " ++
                "recursively defined: " ++ varsToStr (vars))) >> exitFailure
        -- Check types of groups
        -- get list of all types of groups
        let groupTypesList = map (\(Group gtype _ _) -> gtype) allGroups
        -- get list of all valid types

        case (checkIfValidGroupTypes groupTypesList validTypes) of
            Nothing -> putStrLn $ "Group types valid!"
            Just grp@(GroupType name srcpos) -> hPrint stderr (GroupTypesInvalid
                ("Group types invalid on " ++ (show name) ++ " on " ++ (show srcpos))) >> exitFailure


        case (weirdForeachesExist comps) of
            False -> putStrLn $ "Weird foreaches don't exist!"
            True -> hPrint stderr (NonsensicalForeach
                "Foreach doesn't exist: Diagnosis -> Patient -> Doctor") >> exitFailure


        let symbolTableHeaders = buildHeadSymbolTable hdr

        let symTabH = foldl (\ errorOrMap (Group gtype (Var gvar ga gpos) gitems) ->
                case errorOrMap of
                    Left err -> Left err
                    Right hmap -> case (replaceVarsGroup hmap (Group gtype (Var gvar ga gpos) gitems)) of
                        Left err -> Left err
                        Right x -> Right $ HashMap.insert (Var (map toLower gvar) (Annotation "") (newPos "" 1 1))
                            (gtype, x) (hmap)
                )
                (Right symbolTableHeaders) allGroups

        let symbolTableH = case symTabH of
                Left err -> error (show err)
                Right x -> x


        -- check types of groups
        -- every groupvar in [group item] of every group should
        -- have same group type as group
        case (checkIfGroupTypesOfVarsBelong symbolTableH allGroups) of
            Just var@(Var vname _ pos) -> hPutStrLn stderr ("Var " ++ (vname) ++ " in group declaration does not typecheck: " ++ (show pos))  >> exitFailure
            Nothing -> putStrLn "Vars in group declarations typecheck!"

        let expandedGroups = expandGroups symbolTableH
        --Header CHeck
        case mapM (checkValidParams conf symbolTableH ) paramList  of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> putStrLn "All params are of valid types!"

        --Check Group Validity
        case mapM (checkValidGroups conf symbolTableH ) allGroups  of
            Left e -> hPrint stderr e >> exitFailure
            Right r -> putStrLn "All groups contain valid values!"


        let groupsymstring = HashMap.foldrWithKey  (\(Var s _ _) ((GroupType t _),_) p ->
                p++ "\t" ++s ++ "\t" ++ t
                    ++"\t0(group)\n" )  "" symbolTableH
        --check erroneous subfields i.e. whether all fields exist
        case (checkFilters filters filterable) of
            Left e -> hPrint stderr e >>  hPutStrLn stderr "FILTERS:" >>
                print filters >> hPutStrLn stderr "CONF:" >> print filterable >> exitFailure
            Right checkedFilters -> putStrLn "All Fields valid"

        case  mapM (checkFilterTypes conf symbolTableH) filters of
                Left e -> hPrint stderr e >> print filters >> exitFailure
                Right annotatedFilters -> do

                    let newFilters = (populateDefaultValuesFilters (annotatedFilters) (Config confmap))

                    let filtersWithVarsReplaced = (map (replaceVarsFilter symbolTableH) newFilters)

                    let compsResult=   weedComputationList conf comps pos
                    case  compsResult of
                        Left e-> hPrint stderr e >>exitFailure
                        Right (str,ann) -> symTabFun (str++
                            "\nDumping Removed Group Scope at "++
                            (show $ sourceLine pos)++ "\n"
                            ++"\tName\tType\tNesting level\n"
                            ++groupsymstring)
                    let annComps = case  compsResult of
                            Left e-> []
                            Right (str,ann) -> ann

                    putStrLn "Weeded successfully!"
                    return ((Program hdr docs [] (expandedGroups) filtersWithVarsReplaced (annComps)),conf)

weirdForeachesExist :: [Computation Annotation] -> Bool
weirdForeachesExist comps =
    do
        foldl (\acc cur -> case cur of
            Foreach (ForEachFilter fname _) comps1 _ -> if (fname == "diagnosis") then (case (checkHeadForFiltername comps1 "patient") of
                    Nothing -> acc
                    Just comps2 -> case (checkHeadForFiltername comps2 "doctor") of
                        Nothing -> acc
                        Just _ -> True)
                else acc
            _ -> acc
            ) False comps

checkHeadForFiltername :: [Computation Annotation] -> String -> Maybe ([Computation Annotation])
checkHeadForFiltername comps filtername =
    do
        case (head comps) of
            Foreach (ForEachFilter fname _) comps _ -> if fname == filtername then (Just comps) else Nothing

-- get grouptypes of all groupvars for a group
checkIfGroupTypesOfVarsBelong :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> [GroupDefs Annotation] -> Maybe (Var Annotation)
checkIfGroupTypesOfVarsBelong hmap groups =
    do
        let mappedGroups = map (\(Group gtype gvar gitems) -> do

                let varList = filter (\v -> case v of
                        GroupVar x -> True
                        _ -> False) gitems
                foldl (\acc (GroupVar vv@(Var v _ pos)) -> case (HashMap.lookup (Var (map toLower v) (Annotation "") (newPos "" 1 1)) hmap) of
                    Just (gtypeMap, gitemsMap) -> if gtypeMap == gtype then acc else Just vv
                    Nothing -> acc) Nothing varList

                ) groups
        foldl (\acc cur -> case cur of
            Just x -> Just x
            Nothing -> acc) Nothing mappedGroups

        -- all group vars in gitems

        -- check if all group vars exist in symbol table and have same type as gtype


expandGroups :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> [GroupDefs Annotation]
expandGroups symbolTableH =
    do
        map (\(gname, (gtype, gdefs)) -> Group gtype gname gdefs) (HashMap.toList symbolTableH)

checkIfValidGroupTypes :: [GroupType] -> [GroupType] -> Maybe (GroupType)
checkIfValidGroupTypes groupTypes validTypes =
    do
        let dummySrcPos = (newPos "test" 1 1)
        let checkValidity = foldl (\bool grp@(GroupType _ _) ->
                if grp `elem` validTypes then bool else (False, grp))
                (True, GroupType "test"  dummySrcPos) groupTypes

        case checkValidity of
            (False, grp) -> Just grp
            (True, _) -> Nothing

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
        foldl (\(seenVars, repeated) (Var x a pos) ->
            if (x `elem` seenVars) then (x : seenVars, (Var x a pos) : repeated) else
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

replaceVarsGroup :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> GroupDefs Annotation -> Either LexError [GroupItem Annotation]
replaceVarsGroup symbolTableH (Group _ var items) =
    do
        let varList = filter (\v -> case v of
                GroupVar x -> True
                _ -> False) items
        let nonVarList = filter (\v -> case v of
                GroupVar x -> False
                _ -> True) items
        -- Check here for error!
        expandedList <- fmap concat (sequence
                (map (replaceVarsGroupItemGroup symbolTableH) varList))
        Right (nonVarList ++ expandedList)

replaceVarsGroupItem :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> GroupItem Annotation -> [GroupItem Annotation]
replaceVarsGroupItem symbolTableH (GroupVar v@(Var vv a _)) =
    do
        case (HashMap.lookup (Var (map toLower vv) (Annotation "") (newPos "" 1 1)) symbolTableH) of
            Just (t, items) -> items

replaceVarsGroupItemGroup :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> GroupItem Annotation -> Either LexError [GroupItem Annotation]
replaceVarsGroupItemGroup symbolTableH (GroupVar (Var v a _)) =
    do
        case (HashMap.lookup (Var (map toLower v) (Annotation "") (newPos "" 1 1)) symbolTableH) of
            Just (t, items) -> Right $ items
            Nothing -> Left $ NotFoundInSymbolTable ((v) ++ " not declared previously.")

-- Make (Var Annotation) hashable
instance (Hashable (Var Annotation)) where
  hashWithSalt s t@(Var v a _) = s + (hash v)

-- Utility test function to check if symbol table contains a key
testIfHSymbolTableContains :: HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]) -> (Var Annotation) -> IO()
testIfHSymbolTableContains hashmap (Var v a _) =
        case (HashMap.lookup (Var (map toLower v) (Annotation "") (newPos "" 1 1)) hashmap) of
            Nothing -> hPrint stderr "nothing found!"
            Just r -> print ("Found VALUE " ++ show r ++ " for KEY " ++
                v ++ " in symboltable1")

-- Build symbol table from groups
buildHeadSymbolTable :: (Header Annotation) -> HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation])
buildHeadSymbolTable (Header _ args) =
    do
        let keyValuesHeader = map (\(Arg (t) (Var v _ _)) -> ((Var v (Annotation "") (newPos "" 1 1)), (t, []))) (args)
        (HashMap.fromList (keyValuesHeader))

weedGroupFiles::[UseFile]->[String]->Either LexError [UseFile]
weedGroupFiles useList grpFiles =
    do
        let declaredUseFiles = flattenUseFile useList

        if declaredUseFiles == []
            then Right $ useList
            else
                case (null $ filter (not . (`elem` grpFiles)) declaredUseFiles) of
                    False -> Left $ MissingFilesError (
                        "ERROR: Missing one of group files: "
                        ++ ( intercalate ","  declaredUseFiles) ++
                        " out of: " ++ (intercalate "," grpFiles))
                    --Better error messages for other cases. Maybe see what files
                    -- are missing exactly. Doesn't need to be true false exactly
                    True -> Right $ useList


getGroupDefs::IO(String) -> IO([(GroupDefs Annotation)])
getGroupDefs grpFileData =
    do
        readData <- grpFileData
        case parse (manyGroups) "" (removeNewLines readData) of
            Left e -> hPutStrLn stderr ("ERROR: " ++ show e) >> return []
            Right r -> return r

readConfig::String->IO((Config Annotation))
readConfig file =
    do
        program <- readFile file
        path <- getExecutablePath
        readData <- readFile $ (dropFileName path) ++"config.conf"
        let l= lines readData
        let totalMap = configListToMap $ map makeConfig l
        return $ Config totalMap


readDBConfig::String->IO (DBConfig)
readDBConfig file =
    do
        program <- readFile file
        path <- getExecutablePath
        readData <- readFile $ (dropFileName path) ++"database.conf"
        let l= lines readData
        let totalMap = dbConfigListToMap $ map makeDBConfig l
        return $ DBConfig  totalMap

readJoinConfig::String->IO (JoinConfig)
readJoinConfig file =
    do
        program <- readFile file
        path <- getExecutablePath
        readData <- readFile $ (dropFileName path) ++"join.conf"
        let l= lines readData
        let joined =  makeJoinConfig $ concat l
        return $ joined



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
            l -> Left $ MissingConfigField $ "Error. Fields missing in " ++
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

           --does the filter exist in the config file?
            case (M.lookup (filterName, True) conf) of--things to check against for that filter
                Just fieldMap ->  do
                    case ((typeCheckFieldMap fieldMap hmap fieldDefs)) of
                        Left e -> Left e
                        Right r ->  Right $ (Filter filterName r)
                    --trace ("calling f with x = ") (pure( ))

                Nothing -> do
                    case (M.lookup (filterName, False) conf) of
                        Nothing -> Left $ GenError (filterName ++ " is not defined in Config file")
                        Just fieldMapf -> do

                            case ((typeCheckFieldMap fieldMapf hmap fieldDefs)) of
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
                    Left $ TypeError "Is not in the fieldMap"
                Just field ->
                    do
                        case (mapM (compareFieldTypes field (FieldMap fm) hmap) fvalList) of
                            Left e -> do
                                Left e
                            Right r -> do
                                Right $ (FieldDef fieldName r):acc
    ) [] fdList


--given a a group, we need to determine if the subfield exists for any. if yeah, then we check to see if it is in it.
--

getAllFields::(Config Annotation)->((M.Map FieldName (Field Annotation)))
getAllFields (Config conf) =  M.unions $ map (\(_,(FieldMap b )) -> b) (M.toList conf)
    --M.unions (map (FieldMap . snd) (M.toList conf))

checkValidGroups::(Config Annotation)->(HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]))->(GroupDefs Annotation)->Either LexError (GroupDefs Annotation)
checkValidGroups c@(Config conf) symTab (Group gt@(GroupType gtype _) (Var v an pos) (gitem )) =
    do
        let listWithVal = getAllFields c

        case (M.lookup gtype listWithVal) of
            Nothing -> Left $ TypeError (show gtype ++ " is not defined in config file")
            Just g -> case (mapM (compareFieldTypes g (FieldMap listWithVal) symTab )  gitem) of --check the type is valid
                Right r -> Right (Group gt (Var v an pos) gitem )
                Left e -> Left e
        --check getAllFields

checkValidParams
    :: (Config Annotation)
    -> (HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]))
    -> (Arg Annotation)
    -> Either LexError (Arg Annotation)

checkValidParams c@(Config conf) symTab (Arg gt@(GroupType gtype srcpos) var@(Var v an@(Annotation a) pos)) =
    do
        --get list of all fields
        let listWithVal = getAllFields c
        case (M.lookup gtype listWithVal ) of --header name exists in
            Nothing -> Left $ TypeError ("Wait what. How did you get here? Param Not in Symbol Table" ++ show gtype)

            Just j -> Right (Arg gt (Var v an pos))
 {-
            Just j -> case ((compareFieldTypes j (FieldMap listWithVal) symTab ) (GroupVar var)) of --check the type is valid
                Right r -> Right (Arg gt (Var v an))
                Left e -> Left e
-}

--get tye from
--take fieldDefs anda  fieldMap, return an error or a field deaf after calling Field

--check to see if it's in the table.
--if not. welp
--if yes, let's annotate it.

--TYPE RULES COME DOWN TO IT
compareFieldTypes::(Field Annotation)->(FieldMap Annotation)->(HashMap.HashMap (Var Annotation) (GroupType, [GroupItem Annotation]))->(GroupItem Annotation)->Either LexError (GroupItem Annotation)
--field values
compareFieldTypes (FieldValue allValList an@(Annotation fa) ) fm hm gi =
    case gi of
        (GroupValString s sa) -> case ((map toLower s) `elem` allValList) of
                True ->  Right (GroupValString s an) --Right (GroupValString s)
                False-> Left (AllowedValError ("Error. " ++ s ++ " is not defined in the config file list that also contains: " ++ (intercalate "," allValList)))
        --(GroupVar (Var v va)) -> case (an == va) of
          --  True -> Right (GroupVar (Var v va))
           -- False -> Left (AllowedValError ("Var Error while comparing field types" ++ " varaiable " ++ show v ++ ": " ++ show va ++ " versus " ++ show a ))
        (GroupRange (Before i (Annotation a))) ->
            if a == fa then Right $ (GroupRange (Before i (Annotation a)) )
            else Left (TypeError ("ValList Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a) )
        (GroupRange (After i (Annotation a)))->
            if a == fa then Right $ (GroupRange (After i (Annotation a) ))
            else Left (TypeError ("ValList Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))
        (GroupRange (Between i j (Annotation a))) ->
            if a == fa then Right $ (GroupRange (Between i j (Annotation a) ))
            else Left (TypeError ("ValList Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))
        (GroupRange (SingleInt i (Annotation a))) ->
            if a == fa then Right $ (GroupRange (SingleInt i (Annotation a) ))
            else Left (TypeError ("ValList Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))
        (GroupVar var@(Var v (Annotation a) pos)) ->
            if  (HashMap.member (Var (map toLower v) (Annotation "") (newPos "" 1 1)) hm) then Right $ (GroupVar (Var v (Annotation a) pos))
            else Left $ TypeError ("VAL LIST ERROR: " ++ show v ++ "of ann " ++ a ++ " is not in Symbol Table" ++ show hm)
        (GroupDate yy mm dd (Annotation a)) ->
            if ((a == fa) && ((1900 <(yy)) && ((yy) < 2050))  && ((1 <= (mm)) && ((mm) <= 12)) && ((1<= (dd)) && ((dd) <= 31)))
            then Right $ (GroupDate yy mm dd (Annotation a))
            else Left $ (TypeError ("ValList Error with Field Type Mismatch with Date"))
        _ -> Left (AllowedValError ("ValList Error " ++ show fa  ++ show gi ++ show allValList))
--intshow s



compareFieldTypes (FieldType "Int" (Annotation an)) fm hm gr =
    case gr of
        (GroupRange (Before i (Annotation a))) ->
            if a == an then Right $ (GroupRange (Before i (Annotation an)) )
            else Left (TypeError ("Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a) )
        (GroupRange (After i (Annotation a)))->
            if a == an then Right $ (GroupRange (After i (Annotation an) ))
            else Left (TypeError ("Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))
        (GroupRange (Between i j (Annotation a))) ->
            if a == an then Right $ (GroupRange (Between i j (Annotation an) ))
            else Left (TypeError ("Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))
        (GroupRange (SingleInt i (Annotation a))) ->
            if a == an then Right $ (GroupRange (SingleInt i (Annotation an) ))
            else Left (TypeError ("Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))
        gvv@(GroupVar var@(Var v (Annotation a) pos)) ->
            if  (HashMap.member (Var (map toLower v) (Annotation "") (newPos "" 1 1))  hm) then Right $ (GroupVar (Var v (Annotation an) pos))
            else Left $ TypeError ("INTCHECK ERROR : " ++ show v ++ "of ann " ++ a ++ " is not in Symbol Table" ++ show (HashMap.keys hm) )
        g@(GroupValString s (Annotation a)) ->  if (HashMap.member (Var (map toLower s) (Annotation"") (newPos "" 1 1)) hm) then Right g else Left (AllowedValError ("Error Invalid Type" ++ show an  ++ show g ++ "                              " ++ show s ++ "!!!!!!!!!!!!" ++ show hm))


compareFieldTypes (FieldType "String" (Annotation an)) fm hm gv =
    case gv of
        (GroupValString s (Annotation a)) ->
            if a == an then Right $ (GroupValString s (Annotation an))
            else Left (TypeError ("STring Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))
        gvv@(GroupVar var@(Var v (Annotation a) pos)) ->
            if  (HashMap.member (Var (map toLower v) (Annotation "") (newPos "" 1 1))  hm) then Right $   (GroupVar (Var v (Annotation an) pos))
            else Left $ TypeError ("INTCHECK ERROR : " ++ show v ++ "of ann " ++ a ++ " is not in Symbol Table" ++ show (HashMap.keys hm) )
        dat@(GroupDate x y z (Annotation annot)) -> if (annot == an) then Right dat else  Left (AllowedValError ("String Type Error " ++ show an  ++ show gv ++ show hm ))
        _ -> Left (AllowedValError ("String Type Error " ++ show an  ++ show gv ++ show hm ))

compareFieldTypes (FieldType "Date" (Annotation an)) fm hm gd =
    case gd of
        (GroupValString s (Annotation a)) ->
            if a == an then Right $ (GroupValString s (Annotation an))
            else Left (TypeError ("Date Error. Field Type mismatch. Between " ++ show an ++ " And " ++ show a))



compareFieldTypes (FieldVar fv (Annotation an)) fm hm (GroupVar v@(Var gv (Annotation a) pos)) =
    if  (HashMap.member (Var (map toLower gv) (Annotation "") (newPos "" 1 1))  hm) then Right $ (GroupVar (Var gv (Annotation an) pos))
    else Left $ TypeError ("ERROR : " ++ show gv ++ " is not in Symbol Table")
--I need a new type that allows me to pull out the type of var

compareFieldTypes f (FieldMap fm) hm  (GroupVar var@(Var v an pos) ) =
    do
        case (HashMap.lookup (Var (map toLower v) (Annotation "") (newPos "" 1 1)) hm) of
            Nothing -> Left $ TypeError ("ERROR. var " ++ (varToStr var) ++ " not declared " ++ "FIELD IS: " ++ show f ++ " VAR IS : " ++ show var ++ "\n" ++ show hm ++ "\n")

            Just (a,_) -> case (M.member ((groupTypeToStr a)) fm) of
                False -> Left $ TypeError ("ERROR. variable " ++
                        (varToStr var) ++" is used with wrong field")
                True  -> Right (GroupVar (Var v an pos))
compareFieldTypes b fm hm a = Left $ TypeError ("Type Error between " ++
    (show a) ++ " and " ++ (show b))

varToStr::(Var Annotation)->String
varToStr (Var v _ _) = v

varsToStr :: [Var Annotation] -> String
varsToStr vars = intercalate ", " (map (varToStr) vars)

groupTypeToStr::GroupType->String
groupTypeToStr (GroupType s _) = s


events :: [String]
events = ["ct_sim_booked","ct_sim_completed","treatment_compled","patient_arrived","end"]


weedComputationList :: (Config Annotation)->[(Computation Annotation)]-> SourcePos->Either LexError (String,[(Computation Annotation)])
weedComputationList config comps pos = (weedFold config [emptyScope] comps pos)


weedFold :: (Config Annotation)->CompSymTable -> [(Computation Annotation)] ->SourcePos-> Either LexError (String,[(Computation Annotation)])
weedFold conf symtable computations pos =
    do
        let
            errorOrSym = (foldl' (weedEach conf) (Right(symtable,"",[])) computations)
            lineCol = sourceColumn pos
            wrongLinenum = sourceLine pos
            linenum = if lineCol ==1 then wrongLinenum-1 else wrongLinenum
        (newSymtable,internalSymRep,newcops)<-errorOrSym
        Right $ (internalSymRep++"\nDumping Removed Scope at line: "++(show linenum )++ "\n"
                    ++"\tName\tType\tNesting level\n"
                    ++ (stringOfLastScope newSymtable),newcops)



stringOfLastScope :: CompSymTable -> String
stringOfLastScope symtable =
    let
        len = show $ length symtable
        curr= last symtable
    in HashMap.foldrWithKey  (\(Var s _ _) t p -> p++ "\t" ++ s ++ "\t" ++
        (tail $ show t) ++"\t" ++len++ "\n" )  "" curr


weedEach::(Config Annotation)->(Either LexError (CompSymTable,String,[(Computation Annotation)]))
    ->(Computation Annotation)->(Either LexError (CompSymTable,String,[(Computation Annotation)]))
weedEach conf errorOrSym comp =  do
    (sym,ret,compsSoFar) <- errorOrSym
    (newsym,newret,newAnnCom) <- (weedAndTypeCheckComp conf sym comp)
    Right (newsym,ret++newret,compsSoFar++[newAnnCom])

printFold :: CompSymTable -> String
printFold symtable =
    let
        len = show $ length symtable
        curr = last symtable
    in HashMap.foldrWithKey  (\(Var s _ _) t p -> p ++ s ++ "\t" ++ (tail $ show t) ++"\t" ++len++ "\n" )  "" curr



addToSymTable :: CompSymTable -> (Var Annotation) -> ComputationType-> Either LexError CompSymTable
addToSymTable symtable v@(Var name _ pos)  comptype =
    let
        prev = init symtable
        local = last symtable
        errorMayBe = HashMap.lookup v local
        updated = HashMap.insert v comptype local
    in case errorMayBe of
        Nothing->Right$ prev++[updated]
        (Just t) ->  Left$ RedecError $ "'"++name++"' has already been declared in this scope as a " ++ prettyPrint t ++ "in line " ++ (show $sourceLine pos)

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
else Left $ ComputationWrongScope $" Can only be in top scope\nCurrent scope: "++(show symtable)


emptyScope :: HashMap.HashMap (Var Annotation) ComputationType
emptyScope = HashMap.fromList []

weedAndTypeCheckComp :: (Config Annotation) ->(CompSymTable) -> (Computation Annotation)
    -> Either LexError (CompSymTable, String,(Computation Annotation))
weedAndTypeCheckComp conf symtable  (Table variable@(Var name _ pos) constructor  field) =
    evaluateInTopScope symtable check
    where check sym = if ((subFieldExists conf constructor field))
            then
                do
                    newSym <- addToSymTable sym  variable TTable
                    Right $ (newSym,"", Table (Var name (Annotation "Table") pos) constructor  field) --(TFilter constructor)
            else Left . FieldNameError $ "Field "++field++
                " does not belong to " ++ constructor++ "in line " ++ (show $sourceLine pos)
weedAndTypeCheckComp conf symtable (List variable@(Var name _ pos) seqlist) =
    evaluateInTopScope symtable check
    where check sym =
            do
                newSym <- addToSymTable sym  variable TList
                s<-foldl' foldWeedList  (Right $ newSym) seqlist
                Right (s,"", List (Var name (Annotation "List") pos) seqlist)

weedAndTypeCheckComp conf symtable (Barchart variable@(Var name _ pos)) =
    evaluateInTopScope symtable check
    where check sym =
            case getFromSymbolTable sym variable of
                Nothing -> Left . UndefinedVariable $ prettyPrint  variable
                Just TTable ->  Right (sym,"",Barchart (Var name (Annotation $ "Table") pos))
                Just t -> Left . ComputationTypeMismatch $
                            "Cannot draw Barchart of "++ (show variable)
                            ++". It is a " ++ (prettyPrint t) ++ "Not a Table"++ "in line " ++ (show $sourceLine pos)

weedAndTypeCheckComp conf symtable (Print printAction ) =
    weedPrintAction conf symtable printAction
weedAndTypeCheckComp conf symtable (Foreach def comps pos) =
    weedForEach conf symtable comps pos def

weedPrintAction :: (Config Annotation)-> CompSymTable -> (PrintAction Annotation)
            -> Either LexError (CompSymTable, String,(Computation Annotation))
weedPrintAction _ symtable  (PrintVar var@(Var name _ pos)) =
    case getFromSymbolTable symtable var of
            Nothing -> Left . UndefinedVariable $ prettyPrint  name
            Just TIndex -> Left . ComputationTypeMismatch $
                    "Index cannot be printed "++ ((prettyPrint var))++
                    "on line " ++ ((show $sourceLine pos)) ++ "is an index"++ "in line " ++ (show $sourceLine pos)
            Just t -> Right (symtable, "",
                Print $ PrintVar $ Var name (Annotation $ prettyPrint t) pos)

weedPrintAction _ symtable (PrintLength var@(Var name _ pos)) =
    case getFromSymbolTable symtable var of
            Nothing -> Left . UndefinedVariable $ prettyPrint  var++ "in line " ++ (show $sourceLine pos)
            Just TTable ->  Right (symtable, "",
                    Print $ PrintLength $ Var name (Annotation $ prettyPrint TTable) pos)
            Just wrong ->  Left . ComputationTypeMismatch $
                    "Cannot have length of "++ (prettyPrint var)++
                    ". It is a " ++ (prettyPrint wrong) ++ "Not a Table" ++ "in line " ++ (show $sourceLine pos)

weedPrintAction _ symtable (PrintElement tableVar@(Var tname _ tpos) indexVar@(Var iname _ ipos) ) =
    case ((getFromSymbolTable symtable tableVar),
        (getFromSymbolTable symtable indexVar)) of
            (Nothing,_) -> Left . UndefinedVariable $ prettyPrint  tableVar++ "in line " ++ (show $sourceLine tpos)
            (_,Nothing) -> Left . UndefinedVariable $ prettyPrint  indexVar++ "in line " ++ (show $sourceLine ipos)
            (Just TTable, Just TIndex) ->  Right (symtable,"",
                -- TODO: wat
                    Print ( PrintElement (Var tname (Annotation $ prettyPrint $ TTable) tpos)
                        ( (Var iname  (Annotation $ prettyPrint TIndex) ipos) ) ) )
            (Just TTable, Just i)-> Left . ComputationTypeMismatch $
                        "Cannot access "++ (prettyPrint indexVar)++" of table "
                        ++(prettyPrint tableVar)
                        ++". It is a " ++ (prettyPrint i) ++ "Not an Index" ++ "in line " ++ (show $sourceLine tpos)
            (Just t,Just i) -> Left . ComputationTypeMismatch $
                    "Cannot index "++ (prettyPrint tableVar)++". It is a "
                    ++ (prettyPrint t) ++ "Not a Table"++ " in line " ++ (show $sourceLine tpos)

weedPrintAction config symtable  (PrintFilters fields filterVar@(Var fname _ fpos) ) =
    case (getFromSymbolTable symtable filterVar) of
            (Nothing) -> Left . UndefinedVariable $ prettyPrint filterVar++ "in line " ++ (show $sourceLine fpos)
            (Just (TFilter constructor)) ->
                if all (\s -> subFieldExists config constructor s) fields
                then Right (symtable,"",
                    Print (PrintFilters fields (Var fname (Annotation constructor) fpos)))
                else Left . FieldNameError $ "One of the fields "++
                            (show fields)++
                           " does not belong to " ++ constructor++ "in line " ++ (show $sourceLine fpos)
            Just wrong -> Left . ComputationTypeMismatch $
                    "Cannot filter over "++ (prettyPrint filterVar)++
                    ". It is a " ++ (prettyPrint wrong) ++ " Not a Filter"++ "in line " ++ (show $sourceLine fpos)

weedPrintAction _ symtable (PrintTimeLine filterVar@(Var fname _ pos)) =
    case (getFromSymbolTable symtable filterVar) of
            (Nothing) -> Left . UndefinedVariable $ show  filterVar
            (Just (TFilter "patient")) -> Right (symtable,"",
                Print (PrintTimeLine  (Var fname (Annotation "patients") pos)))
            (Just (TFilter "patients")) -> Right (symtable,"",
                Print (PrintTimeLine  (Var fname (Annotation "patients") pos)))
            Just wrong -> Left . ComputationTypeMismatch $
                    "Cannot print TimeLine of "++ (prettyPrint filterVar)++
                    ". It is a " ++ (prettyPrint wrong) ++ " Not a patient" ++ "in line " ++ (show $sourceLine pos)

weedForEach :: (Config Annotation)->CompSymTable -> [(Computation Annotation)] ->SourcePos-> (ForEachDef Annotation)
    ->Either LexError (CompSymTable,String,(Computation Annotation))
weedForEach conf symtable newcomp pos (ForEachFilter filterName var@(Var iname _ ipos) )  =
    if (fieldExists conf filterName)
    then do
            fname <- isValidInNested conf symtable filterName pos
            newsym <- ( addToSymTable (symtable++[emptyScope])
                                    var (TFilter fname) )
            (intSymRep,annComps)<-  (weedFold conf newsym newcomp pos)

            Right (symtable,intSymRep,(Foreach (ForEachFilter filterName (Var
                                 iname (Annotation filterName) ipos)) annComps pos) )

    else Left $  FieldNameError $ ""++
            filterName++" is not a valid loopable Filter at line " ++ (show $sourceLine pos)

weedForEach config symtable newcomp pos (ForEachTable indexVar@(Var iname _ ipos) tableVar@(Var tname _ tpos) )  =
    evaluateInTopScope symtable (\sym->
        case getFromSymbolTable sym tableVar of
            Nothing -> Left . UndefinedVariable $ prettyPrint  tableVar
            Just TTable -> do
                newsym <- (addToSymTable  (sym++[emptyScope]) indexVar TIndex)
                (intSymRep,annComps)<-(weedFold config newsym newcomp pos)
                Right (sym,intSymRep, (Foreach  (ForEachTable (Var iname (Annotation "index") ipos)
                                        (Var tname (Annotation "Table") tpos)) annComps pos) )
            Just t-> Left ( ComputationTypeMismatch $
                    "Cannot go through loop for "++ (prettyPrint tableVar)
                    ++". It is a " ++ (prettyPrint t) ++ "Not a Table at scope ending in line " ++ (show $sourceLine pos)
                ))
weedForEach config symtable newcomp pos (ForEachSequence memberVar@(Var mname _ srcpos) undefSequence) =
    evaluateInTopScope symtable check
    where check sym=
            do
                newsym <- (addToSymTable (sym++[emptyScope]) memberVar TSequence)
                s <- foldl' foldWeedList (Right newsym) undefSequence
                (intSymRep,annComps) <- (weedFold config s newcomp pos)

                Right (sym,intSymRep, (Foreach((ForEachSequence  (Var mname (Annotation "member") srcpos)

                 undefSequence) ) annComps pos))


weedForEach config symtable newcomp pos (ForEachList memberVar@(Var mname _ posM) listVar@(Var lname _ posL) )  =

    evaluateInTopScope symtable check
    where check sym=
            case getFromSymbolTable sym listVar of
                Nothing -> Left . UndefinedVariable $ prettyPrint  listVar
                Just TList -> do
                    newsym <-(addToSymTable (sym++[emptyScope])   memberVar TSequence)
                    (intSymRep,annComps) <- (weedFold config newsym newcomp pos)
                    Right (sym,intSymRep,Foreach (ForEachList (Var mname  (Annotation"member") posM)
                                 (Var lname (Annotation"List") posL)) annComps pos)

                Just t-> Left ( ComputationTypeMismatch $
                        "CAnnot Go through loop for "++ (prettyPrint listVar)++
                        ". It is a " ++ (prettyPrint t) ++ "Not a List at scope ending in line " ++ (show $sourceLine pos))


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

isValidInNested ::(Config Annotation)-> CompSymTable -> FilterName -> SourcePos-> Either LexError FilterName
isValidInNested conf symtable filterName pos =
    let
        filtersUsed = (findAllFilters symtable)
        noFilters = null filtersUsed
        topScope = isNowInTopScope symtable
        prevUsed = (elem filterName filtersUsed) || (elem (init filterName) filtersUsed) || (elem (filterName++"s") filtersUsed) --patient or patients
    in case ((noFilters && topScope), ( (not noFilters)&&(not prevUsed))) of
        (True,_)-> Right filterName
        (False,True) -> Right filterName
        (False,False)->Left $ComputationWrongScope $ " filtered forloop is invalid at scope ending  line " ++ (show $ sourceLine pos)
        --(_,False)->Left $ComputationWrongScope $ filterName ++ " is already being looped over in an outerscope ending at line " ++ (show $ sourceLine pos)


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
