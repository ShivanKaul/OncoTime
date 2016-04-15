{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen where

import Types
import Lexer
import Data.List
import Parser
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char
import Text.Parsec.String
import Text.Regex
import Debug.Trace

{-TODO
1. Get actual fucking sql queries to be generated properly
2. WRite teh javascript code for:
    a. For Each  - NOT SURE IF IT WORKS
    c. barchart

3. Should there be several different queries? One for each filter? That may make more sense

 -}

generateSQL :: (Program Annotation)->DBConfig ->(Config Annotation)->JoinConfig-> String
generateSQL program@(Program header docs usefilelist groups filt comps) dbconf weedconf joinconf =
    do
        -- let diagnosis = (checkIfDiagnosis filt)
        -- let query = generateQueries filt dbconf diagnosis
        let queries = generateComputations filt weedconf dbconf joinconf comps
        let scaff = generateScaffoldingJS

        let queryElements = (getQueryElements comps)

        --let st = generateScaffoldingJS query (getQueryElements comps) dbconf
        let computationFunctions = generateForEachFunctions 

        let helperFunctions = generateSortFunction

        scaff ++ queries  ++ computationFunctions ++ helperFunctions 
        
        --stdbconf
        -- let displayFunction = generateDisplayFunction comps  dbconf weedconf diagnosis
        -- generateScaffoldingJS query displayFunction
        
generatePrettyRowFunction :: String
generatePrettyRowFunction = "function generatePrettyRow(row) {\n\
            \ \treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"

getNameInDatabase :: DBConfig -> String -> String
getNameInDatabase (DBConfig dbconf) name =
    case M.lookup name dbconf of
        Just a -> a
        Nothing -> error $ name ++ " is not a valid name in " ++ show dbconf

checkIfDiagnosis :: [Filter Annotation] -> Maybe [String]
checkIfDiagnosis filters =
    do
        foldl (\acc cur@(Filter _ fdefs) -> foldl (\accInner (FieldDef fname fvals) ->
            if fname == "diagnosis" then
                case fvals of
                    [GroupWildcard] -> accInner
                    _ -> Just (map (\(GroupValString name _) -> name) fvals)
            else
                accInner) acc fdefs)
                 Nothing filters


generateComputations::[Filter Annotation]->Config Annotation-> DBConfig->JoinConfig-> [Computation Annotation]-> [String]
generateComputations filterList conf ( dbconfmap@(DBConfig dbconf)) joinconf comps = 
    do
        map (generateComps filterList conf dbconfmap joinconf) comps

--generates each individual query and computation
generateComps::[Filter Annotation]->Config Annotation->DBConfig->JoinConfig->Computation Annotation-> String
generateComps filterList conf (dbconfmap@(DBConfig dbconf)) joinconf comp = 
    do
        let selectStatement = genFullSQLStatement filterList conf dbconfmap joinconf comp

        let generatedCode = case comp of
                (Foreach def compList _) -> 
                    case def of
                        ForEachFilter filtName v -> genForEachFilter ++ "foreach_filter(rows, \""++ (dbconf `getNameInDatabase` (filtName ++ "_loop")) ++"\", " ++ filtName++ "_functions, "filtName++"_args);\n" ++ "});\n }"
                        _ -> ""
                (Table v filtName fieldName) -> "SHOULD BE SUPPORTED"
                (List v seqFieldList) -> ""
                (Print p) -> ""
                (Barchart v) -> "CAN'T EXIST WITH NO SCOPE"


        let code = genFullDBQuery selectStatement generatedCode
        code

--select Patient.*, PatientDoctor.*, Diagnosis.* from Patient JOIN PatientDoctor On PatientDoctor.PatientSerNum = Patient.PatientSerNum JOIN Diagnosis on Patient.PatientSerNum = Diagnosis.PatientSerNum limit 100;


getQueryElements::[Computation Annotation]->[String]
getQueryElements comp = 
    do
        case comp of
             (Foreach def compList _)->
                case def of
                    (ForEachFilter filtName v) -> (map toLower filtName):(accumulateForEach compList dbconfmap)
                    _ -> []
             (Table v filtName fieldName) -> filtName:[]
             _ -> []

genFullSQLStatement::[Filter Annotation]->Config Annotation->DBConfig->JoinConfig->Computation Annotation->String
genFullSQLStatement filterList conf dbconfmap joinconf comp = 
    do
        let listOfQueryElements = getQueryElements comp
        --separate the fields from the filters
        let fieldNameList = getFieldNameList filterList listOfQueryElements 

        --separate the filters from the filterList
        let filterNameList = getFilterNameList filterList listOfQueryElements


        --get select from
        let selectStmt = genSelectStatements dbconfmap joinconf filterNameList fieldNameList 

        --get Joins
        let joinStmt = genJoinStatements dbconfmap joinconf filterNameList fieldNameList

        --get wheres
        let whereStmt = genWhereStatements filterList dbconfmap joinconf filterNameList fieldNameList
        
        --concat
        --if(length whereStmt > 0) then selectStmt ++ joinStmt ++ whereStmt 
          --  else selectStmt ++ (intercalate " ," filterNameList )
        --"DIAGNOSTIC: " ++"filterList: " ++ show filterNameList ++ "\n" ++ "select Statement: " ++ selectStmt ++ "\n" ++ "Join Statement: " ++ joinStmt ++ "\n" ++ "where statement: " ++  whereStmt ++ "\n"
        selectStmt ++  joinStmt  ++  whereStmt 





--FINISH THE THIS BY 9

--get all the filterNames
--go through the fitler list.
getFilterNameList::[Filter Annotation]->[String]->[FilterName]
getFilterNameList filtList queryElements = filter (\filtName-> filtName `elem` queryElements) (getFilterNames filtList)


--get all the fieldnames 
getFieldNameList::[Filter Annotation]->[String]->[FieldName]
getFieldNameList filtList queryElements = filter (\filtName-> filtName `elem` queryElements) (getFieldNames filtList)


getFilterNames::[Filter Annotation]->[FilterName]
getFilterNames filts = map (\(Filter filtName fdefs) -> filtName) filts


getFieldNames::[Filter Annotation]->[FieldName]
getFieldNames filts = 
    do
        let listOfFieldDefs  = concat (map (\(Filter filtName defList) -> defList) filts) 
        let fieldNames = map (\(FieldDef fname fval)-> fname) listOfFieldDefs
        fieldNames


getFieldDefs::[Filter Annotation]->[FieldDef Annotation]
getFieldDefs filtList = concat ( map (\(Filter fname fdefs )-> fdefs)filtList)

genSelectStatements::DBConfig->JoinConfig->[String]->[String]->String
genSelectStatements db@(DBConfig dbconf) joinconf filterNameList fieldNameList = 
    do
        let selectStmt = case (length filterNameList) > 0 of
             True -> case (length fieldNameList) > 0of
                 True -> (intercalate ", " (map (\filt-> (db `getNameInDatabase` filt) ++ ".*") filterNameList)  ) ++ ", " ++ (intercalate ", " (map (\fie-> (db `getNameInDatabase` fie) ++ ".*") fieldNameList))  

                 False -> (intercalate ", " (map (\filt-> (db `getNameInDatabase` filt) ++ ".*") filterNameList)  ) 
             False -> case (length fieldNameList) > 0 of
                 True -> (intercalate ", " (map (\fie-> (db `getNameInDatabase` fie) ++ ".*") fieldNameList)) 
                 False -> "*"
       
        "select " ++ selectStmt ++ " from "


--JOIN PatientDoctor On PatientDoctor.PatientSerNum = Patient.PatientSerNum JOIN Diagnosis on Patient.PatientSerNum = Diagnosis.PatientSerNum limit 100;

--NEED TO EXCLUDE THE FIELD ELEMENTS FROM THE LIST OF THINGS IN THE FILTERLIST

genJoinStatements::DBConfig->JoinConfig->[String]->[String]->String
genJoinStatements db joinconf [] [] = ""
genJoinStatements db@(DBConfig dbconf) (JoinConfig jointo joinableList) [] (fieldNameHead:fieldNameList) =  --case where one exist
    do
        let dbHead = (\filt-> (db `getNameInDatabase` filt)) fieldNameHead
        let dbFields = map  (\filt-> (db `getNameInDatabase` filt)) fieldNameList 
        let joinStatement = case (length (fieldNameHead:fieldNameList)) > 1 of
             True -> 
                 do
                    dbHead ++ concat ( map (\field -> " JOIN " ++ field ++ " ON " ++  field ++ "."++ jointo ++ " = " ++ dbHead ++ "."++jointo ++ " "   ) dbFields )
             False -> ""
        joinStatement
genJoinStatements db@(DBConfig dbconf) (JoinConfig jointo joinableList) (filterNameHead:filterNameList) []  =  --case where one exist
    do
        let dbHead = (\filt-> (db `getNameInDatabase` filt)) filterNameHead
        let dbFields = map  (\filt-> (db `getNameInDatabase` filt)) filterNameList
        let joinStatement = case (length (filterNameHead:filterNameList)) > 1 of
             True -> 
                 do
                    dbHead ++ concat ( map (\field ->" JOIN " ++ field ++ " ON " ++  field ++ "."++ jointo ++ " = " ++ dbHead ++ "."++jointo ++ " "   ) dbFields )
             False -> ""
        joinStatement
genJoinStatements db@(DBConfig dbconf) (JoinConfig jointo joinableList) filtList@(filterNameHead:filterNameList) fieldList@(fieldNameHead:fieldNameList) =  --case where both exist
    do
        let joinStatement = case (length (filtList) > 1) of
             True -> 
                 do 
                    let dbHead = (\filt-> (db `getNameInDatabase` filt)) filterNameHead
                    let dbFields = (map  (\filt-> (db `getNameInDatabase` filt)) filterNameList) ++ (map  (\fie-> (db `getNameInDatabase` fie)) fieldList )
                    dbHead ++ concat ( map (\field -> " JOIN " ++ field ++ " ON " ++  field ++ "." ++ jointo ++ " = " ++ dbHead ++ "."++jointo ++ " "   ) dbFields )

             False -> case (length fieldList) > 1 of
                True -> 
                    do
                        let dbHead = (\filt-> (db `getNameInDatabase` filt)) fieldNameHead 
                        let dbFields = (map  (\filt-> (db `getNameInDatabase` filt)) filterNameList) ++ (map  (\fie-> (db `getNameInDatabase` fie)) fieldList )
                        dbHead ++ concat ( map (\field -> " JOIN " ++ field ++ " ON " ++  field ++ "." ++ jointo ++ " = " ++ dbHead ++ "."++jointo ++ " "   ) dbFields )
                False -> 
                    do
                        let dbHead = (\filt-> (db `getNameInDatabase` filt)) filterNameHead
                        let dbFields = (map  (\fie-> (db `getNameInDatabase` fie)) fieldList )
                        dbHead ++ concat ( map (\field -> " JOIN " ++ field ++ " ON " ++  field ++ "."++ jointo ++ " = " ++ dbHead ++ "."++jointo ++ " "   ) dbFields )
                         
        joinStatement

genWhereStatements::[Filter Annotation]->DBConfig->JoinConfig->[String]->[String]->String
genWhereStatements filtList db@(DBConfig dbconf) jc@(JoinConfig jointo joinableList) filterNameList fieldNameList = 
    do
        --if a filter, get all
        let filterWhere = (filter (not . null) (map (genWhereFilter filtList db jc fieldNameList) filterNameList))
        -- if a field, 
        let fieldWhere = (filter (not . null) (map (genWhereField filtList db jc) fieldNameList))

        case (length filterWhere > 0) of
            True -> case (length fieldWhere > 0) of
                True -> " where " ++ (intercalate " AND " filterWhere) ++ " AND " ++ "(" ++ (intercalate " AND " fieldWhere) ++ ")"
                False -> " where " ++ (intercalate " AND " filterWhere)
            False ->case (length fieldWhere > 0) of
                True-> " where " ++ (intercalate " AND " fieldWhere)
                False-> "" 

--TODOOOOO
genWhereField::[Filter Annotation]->DBConfig->JoinConfig->String->String
genWhereField filtList dbconf joinconf fieldName = 
    do 
        let sqlName = (dbconf `getNameInDatabase` fieldName)
        let fieldDefList = (getFieldDefs filtList) --get the fields associated with that fieldmap
        let field = case (find (\(FieldDef fname fdef) -> fname == fieldName ) fieldDefList) of
             Just m -> m
             Nothing -> error  ("CODEGEN ERROR: " ++ fieldName ++ " Not in list of fields" ++ show filtList)

        case (fieldDefToWhere sqlName dbconf field) of
            [] -> ""
            st ->  st

genWhereFilter::[Filter Annotation]->DBConfig->JoinConfig->[FieldName]->FilterName->String
genWhereFilter filtList db@(DBConfig dbconf) joinconf fieldNameList filterName =
    do
        let fields = case (find (\(Filter filtName fdef) -> filterName == filtName) filtList) of
             Just m -> m
             Nothing -> error  ("CODEGEN ERROR: " ++ filterName ++ " Not in list of filters" ++ show filtList)

        let sqlName = (db `getNameInDatabase` filterName)
        let listOfConditions = filter (not . null) (filterToWhere fields db sqlName fieldNameList)

        if(null listOfConditions) then "" else "(" ++ intercalate " OR " listOfConditions ++ ")"


filterToWhere::Filter Annotation->DBConfig->String->[FieldName]->[String]
filterToWhere (Filter filt fdef) db@(DBConfig dbconf) sqlName fieldNameList = 
    do
        let fieldDefsExcludingDeclaredFields = filter (\(FieldDef fname fvalList) -> not (fname `elem` fieldNameList)) fdef 
        map (fieldDefToWhere sqlName db) fieldDefsExcludingDeclaredFields


fieldDefToWhere::String->DBConfig->FieldDef Annotation->String
fieldDefToWhere sqlName db@(DBConfig dbconf) (FieldDef fname fvals) = 
    do
        (intercalate " OR " (map (fieldValToWhere (sqlName ++ "." ++ (db `getNameInDatabase` (fname ++ "SQL") ))) fvals))
        

fieldValToWhere::String->FieldVal Annotation->String
fieldValToWhere sqlName (GroupValString str an ) = "( " ++ sqlName  ++ " like \"" ++ str ++ "%\"" ++ " )" 
fieldValToWhere sqlName (GroupRange (Before i a )) = "( " ++ sqlName ++ " <" ++ show i ++ " )" 
fieldValToWhere sqlName (GroupRange (After i a )) = "( " ++ sqlName ++ "> " ++ show i ++ " )" 
fieldValToWhere sqlName (GroupRange (Between i j a )) = "( " ++ sqlName ++ " > " ++ show i ++ " AND " ++ sqlName ++ "< " ++ show j ++ " )" --HOW?
fieldValToWhere sqlName (GroupRange (SingleInt i a)) = "( " ++ sqlName ++ "= " ++ show i ++ " )" 
fieldValToWhere sqlName (GroupDate dd mm yy an) = "( " ++ "HOW?" ++ " )" -- HOW?
fieldValToWhere _ _ = "" 




genFullDBQuery::String->String->String
genFullDBQuery selectStatement  genCode = 
    do
        let dbQueryLeft = "\t\tdb.query('"
        let dbQueryRight = "', function(err, rows, fields) {\n\
            \\t\t\tif (err) throw err;\n"
        let dbQueryEnd = "\n" --});" --This goes around each one?
        dbQueryLeft ++ selectStatement ++ dbQueryRight ++ genCode ++ dbQueryEnd




--HOW DO VARIABLES FIT INTO THIS???

--how do I generate these foreachs? Espcially foreach within foreach

--STILL DOESN'T WORK, I DON'T THINK. IS THERE AN ALTERNATE STRATEGY?
--SOMEHOW I NEED TO DEAL WITH THE SCOPE OF VARIABLES?
--I GueSS USE THE VARIABLES ANNOTATION

genForEachFilter::DBConfig->Computation Annotation->String
{-genForEachFilter (Foreach def compList _) = "function(rows){\n fns = {\n" ++ (intercalate "," (map genForEachFilter compList)) ++ ")\n};\n rows.forEach(function(entry){ fns.forEach(function(func){ func(rows)\n}\n}\n }" -}
genForEachFilter dbconfmap (Foreach def compList _) = 
    case def of
        ForEachFilter filtName v -> "\t(function(row){\n\t" ++ filtName++"_fns = [\n" ++  (intercalate ",\t\n" (map (genForEachFilter dbconfmap) compList)) ++"\n\t ]\n" ++ "\t for(j =0; j < "++ filtName ++ "_fns.length; j++){ \n\
    \ \t\t" ++ filtName++ "_fns[j](row); \
    \ \n}\n })"
        ForEachTable v v2 -> "NOT SUPPORTED"
        ForEachSequence v seqfield ->"NOT SUPPORTED"
        _ -> "NOT SUPPORTED"
genForEachFilter _ (Table v fil fie) = ""
genForEachFilter db (Print p ) = genPrintInForeach p db
genForEachFilter _ (Barchart v) = ""
genForEachFilter _ (List v seqList) = ""


--How do I generate all these print statements?
--use the var given!!!
genPrintInForeach::PrintAction Annotation ->DBConfig->String
genPrintInForeach (PrintVar (Var v (Annotation an))) db = "function PrintVar("++ v ++"){console.log("++ v ++"[\"" ++ (db `getNameInDatabase`((map toLower an) ++ "SQL"))++ "\"])}"--vanilla case
genPrintInForeach (PrintLength v) _ = ""--count???
genPrintInForeach (PrintFilters filts v@(Var varName an)) db = "function PrintFilters(row){" ++ (genPrintFilterString v filts db) ++ "console.log(" ++ varName ++")}"--like print id,sex of. --needs to be anonymous, otherwise I can't do it 
genPrintInForeach (PrintElement (Var index a) (Var tab an)) _ = "function PrintElement("++ index ++ ", "++ tab ++"){console.log("++ tab ++"["++index++ "])}"

genPrintFilterString::(Var Annotation)->[FilterName]->DBConfig->String
genPrintFilterString (Var v an) filtList (DBConfig dbconf)= v ++ " = {" ++ (intercalate ",\n" (map (\filt -> filt ++ " : row." ++ (dbconf M.! filt)) filtList )) ++ "\n};"


genWhereClause::[Filter Annotation]->DBConfig->[FilterName]-> String
genWhereClause filterList dbconfmap filterNameList = 
    do --generate a where clause, where if diagnoses is in it, then we include it
        let joinClause = case "diagnosis" `elem` (filterNameList) of
                True -> 
                    do
                        case checkIfDiagnosis filterList of
                            Nothing -> ""
                            Just diags -> (generateWhereClauseForDiag diags( dbconfmap))
                False -> ""

        let actuallyUsed = filter (\(Filter filtName fdefList) -> filtName `elem` filterNameList)


        whereStatement <- map (\(Filter filtName fdefList) ->
            do
                        --get list of fields without wildcard
                let filterFieldsWithoutWildcard = filter (\(FieldDef fname fieldvals) ->
                        case fieldvals of
                            [fval] -> fval /= GroupWildcard
                            _ -> fname /= "diagnosis") (fdefList)
                
                if (length filterFieldsWithoutWildcard) == 0 then ""
                else do
                    let whereQuery = joinClause ++
                                    (generateWhereClauses ( dbconfmap) filterFieldsWithoutWildcard filtName)
                            -- regex to replace all AND AND by AND
               
                    let regexedWhere = subRegex (mkRegex " OR[ )]+AND ") whereQuery ") AND "
                            -- Hack for getting rid of last AND
                    (T.unpack (T.dropEnd 5 (T.pack regexedWhere)))
                ) filterList
        (joinClause ++ whereStatement)
            
{-functions[j].apply(this,args[j]) }
--gets names of all the filters used
--Doesn't support arbitrary nesting of doctors
accumulateForEach::[Computation Annotation]->[String]
accumulateForEach compList= 
    do
        let filts = filter (isForEachFilter) compList
        let filtNames = map (\(Foreach (ForEachFilter fname _) _ _)  -> (map toLower fname)) filts
        filtNames 
-}
accumulateForEach::[Computation Annotation]->DBConfig->[String]
accumulateForEach [] _ = []
accumulateForEach ((Foreach (ForEachFilter fn v) compList  spos):xs) db = fn : (accumulateForEach xs db) ++ (accumulateForEach compList db) 
accumulateForEach ((Foreach (ForEachTable (Var v1 a1) (Var v2 a2)) compList  spos):xs) db = (db `getNameInDatabase` v2) : (accumulateForEach xs db) ++ (accumulateForEach compList db) 
accumulateForEach (x:xs) db= (accumulateForEach xs db)  

isForEachFilter::(Computation Annotation) -> Bool
isForEachFilter (Foreach (ForEachFilter _ _) _ _ ) =  True
isForEachFilter _ = False

--generateQuery::[Filter Annotation]->DBConfig->String
--generateQuery filterList ( dbconfmap@(DBConfig dbconf)) = 
--  do

generateQueries::[Filter Annotation]->DBConfig-> Maybe [String] -> [String]
generateQueries filterList ( dbconfmap@(DBConfig dbconf)) diag =
    do
        let columns = case diag of
                Nothing -> "*"
                _ -> "Patient.*, Diagnosis.Description"
        let queryString = "select " ++ columns ++ " from "
        --iterate through filter list
        queryList <- map (\(Filter filtName fdefList) ->
            do
                if filtName /= "population"
                    then "/*"++filtName++" filtering has not been implemented yet, sorry! */"
                --then queryString ++ (dbconf M.! filtName) --THIS IS PART OF WHAT BRENDAN DID
                else
                     do
                        let fromQuery = case diag of
                                Nothing -> (dbconfmap `getNameInDatabase` filtName)
                                _ -> (dbconfmap `getNameInDatabase` filtName) ++ ", Diagnosis"
                        let joinClause = case diag of
                                Nothing -> ""
                                Just diagnoses -> generateWhereClauseForDiag diagnoses ( dbconfmap)
                        let selectQuery = queryString ++ fromQuery
                        --get list of fields

                        --get list of fields without wildcard
                        let filterFieldsWithoutWildcard = filter (\(FieldDef fname fieldvals) -> case fieldvals of
                                [fval] -> fval /= GroupWildcard
                                _ -> fname /= "diagnosis") (fdefList)

                        --form the query
                        if (length filterFieldsWithoutWildcard) == 0 then selectQuery
                        else do
                            let whereQuery = " where " ++ joinClause ++
                                    (generateWhereClauses ( dbconfmap) filterFieldsWithoutWildcard filtName)
                            -- regex to replace all AND AND by AND
                            let regexedWhere = subRegex (mkRegex " OR[ )]+AND ") whereQuery ") AND "
                            -- Hack for getting rid of last AND
                            selectQuery ++  (T.unpack (T.dropEnd 5 (T.pack regexedWhere)))
                ) filterList
        return queryList

generateWhereClauseForDiag :: [String] -> DBConfig -> String
generateWhereClauseForDiag diagnoses ( dbconfmap) =
    do
        let prefixDiags = "(Patient.PatientSerNum = Diagnosis.PatientSerNum) AND ("
        let whereDiags = map (\diagnosis -> "Diagnosis.DiagnosisCode like " ++
                "\"" ++ (dbconfmap `getNameInDatabase` diagnosis) ++ "%\"") diagnoses
        prefixDiags ++ (intercalate " OR " whereDiags) ++ ") AND "


generateWhereClauses :: DBConfig->[FieldDef Annotation] -> String -> String
generateWhereClauses ( dbconfmap) fielddefs filtername =
    do

        foldl (\acc (FieldDef fname fvals) ->
            let
                tname = (dbconfmap `getNameInDatabase` filtername) ++ "." ++ (dbconfmap `getNameInDatabase` fname)
            in acc ++" ("
                ++  (generateFieldValsForWhere fvals tname) ++ ") AND ") "" fielddefs


generateFieldValsForWhere :: [FieldVal Annotation] -> String -> String
generateFieldValsForWhere fvals fname =
    do
        let expanded = foldl (\acc fval -> case fval of
            -- TODO: Handle multiple for both of these by having commas
                GroupValString str _ -> acc ++ (fname) ++ " like \"" ++ str ++ "%\" OR  "
                GroupRange (SingleInt i _) -> acc ++ fname ++ " = " ++ (show i) ++ " OR "
                GroupRange (Before i _) -> acc ++ fname ++ " < " ++ (show i) ++ " OR "
                GroupRange (After i _) -> acc ++ fname ++ " > " ++ (show i) ++ " OR "
                GroupRange (Between i1 i2 _) -> acc ++ fname ++ " > " ++ (show i1) ++
                        " AND " ++ fname ++ " < " ++ (show i2) ++ " OR "
                (GroupDate dd mm yy _) -> acc ++ fname ++" " ++ (show dd) ++"-"++ (show mm) ++"-"++ (show yy) ++ " OR "
                ) "" fvals
        expanded


generateScaffoldingJS ::String -- -> String
generateScaffoldingJS = --funcs=  -- dbDisplayFunction =
    do
        let mysqlReq = "var mysql = require('mysql');\n"
        let tableReq = "var Table = require('cli-table');\n"
        let config = "var db = mysql.createConnection({\n\
                \\thost: 'localhost',\n\
                \\tuser: '520student',\n\
                \\tpassword: 'comp520',\n\
                \\tdatabase: 'oncodb',\n\
                \\tport: 33306\n\
            \});\n"
        mysqlReq ++ tableReq ++ config

{-
generateScaffoldingJS :: [String] ->[String]->DBconfig String -- -> String
generateScaffoldingJS dbQueryList queryElements dbconf = --funcs=  -- dbDisplayFunction =
    do
        let mysqlReq = "var mysql = require('mysql');\n"
        let tableReq = "var Table = require('cli-table');\n"
        let config = "var db = mysql.createConnection({\n\
                \\thost: 'localhost',\n\
                \\tuser: '520student',\n\
                \\tpassword: 'comp520',\n\
                \\tdatabase: 'oncodb',\n\
                \\tport: 33306\n\
            \});\n"

        let dbConnect = "db.connect(function(err) {\n\
                \\tif (err) console.log(err);\n\
                \\telse {\n"
        {-
        let dbQueryLeft = "\t\tdb.query('"
        let dbQueryRight = "', function(err, rows, fields) {\n\
                \\t\t\tif (err) throw err;\n"
        let dbDisplay = "" -- "\t\t\tconsole.log(display(rows).toString());\n\
            -- \\t\t});\n" -- \\t}\n"
        -}
       -- let dbEnd = "\tdb.end();\n\" "\});\n\n"
       --
        let dbEnd = "\t\ndb.end();\n});\n\n "

        let dbDisplayFunctionStart = "function display(rows) {\n"
        let dbDisplayFunctionEnd = "}\n"

--        let formatQueryList = map (\x -> dbQueryLeft ++ x ++ dbQueryRight ++
  --              dbDisplay ++ "\n") dbQueryLis/t


        let formatQueryList = map (\x -> x ++ "\n") dbQueryList
        
        mysqlReq ++ tableReq ++ config ++ dbConnect ++ (concat formatQueryList) ++ 
            dbEnd ++ generateDisplayPrintFunction ++ "\n" ++ generateBarchartFunction ++ "\n" ++ generateForEachFunctions queryElements dbconf ++ "\n" ++ generateCountKeyFunction ++"\n" ++ generateDisplayTable
            -- generatePrettyRowFunction ++ dbDisplayFunctionStart ++ dbDisplayFunction ++ dbDisplayFunctionEnd
--

-}

generateDisplayPrettyFunction::String
generateDisplayPrettyFunction = "function generatePrettyRow(row) {\n \treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"

--generates all necessary print functions
generateDisplayPrintFunction::String
generateDisplayPrintFunction = "function print_var(row) {\n \t console.log(row)\n\
        \}\n\n"


generateSortFunction::String
generateSortFunction =  "function sortObj(list, key) { 
\ //Taken from http://stackoverflow.com/questions/2466356/javascript-object-list-sorting-by-object-property \n \
\ \t   function compare(a, b) { \n\
\  \t\t      a = a[key]; \n\
\  \t\t     b = b[key]; \n\
\  \t\t    var type = (typeof(a) === 'string' || \n\
\  \t\t                typeof(b) === 'string') ? 'string' : 'number'; \n\
\ \t\t      var result; \n\
\   \t\t   if (type === 'string') result = a.localeCompare(b); \n\
\    \t\t  else result = a - b; \n\
\     \t\t return result; \n\ 
\   \t } \n\
\   \t return list.sort(compare); \n\
\  } \n "


--TODO: THIS IS THE LAST PART. BASICALLY. JUST NEED TO DO THIS A BIT, TEST IT,
--AND GET TERTIARY STUF WORKING
--DO I PASS IN FUNCTIONS IN JAVASCRIPT? OR HERE?


--ARGS IS AN ARRAY OF ARRAYS, where each subarray contains the arguments for it.
--indexed by function

generateForEachFunctions::String
generateForEachFunctions =  "function foreach_filter(rows, key, functions, args){ \n 
\ \t sortedRows = sortObj(rows, key) \n \
\ \t arrOf = new Array() \n\
\ \t prev_index = 0; \n \
\ \t for(i = 0; i < rows.length; i++){ \n \
\ \t\t   if(arrOf.length == 0){  \n \ 
\  \t\t\tarrOf[prev_index] = new Array() \n \
\ \t\t\t arrOf[prev_index].push(sortedRows[doctor_i]) \n \
\ \t\t\t  } else if(arrOf[prev_index][0][key] == sortedRows[i][key] ){ \n \
\ \t\t\t arrOf_doctor[prev_index].push(sortedRows[i]) \n \
\ \t\t\t } else { \n \  
\ \t\t\t for(j = 0; j< functions.length; j++) {
\ \t\t\t args[j][0] = arrOf_doctor[prev_index] \n \ --THE first argument is ALWAYS the row
\ \t\t\t  functions[j].apply(this,args[j]) } \n \
\ \t\t  prev_index ++ \n \
\ \t\t	arrOf[prev_index] = new Array() \n \
\ \t\t  arrOf[prev_index].push(sortedRows[i]) \n \
\ \t } \n \
\ \t } \n \
\ } "


--PRev solution: In Haskell code gen.
-- \       //FUNCTIONS \n \ "  ++ (intercalate '\n\t' functions) ++ "\t\t	prev_index++ \n \


--code check for an additional argument, it being the arguments that are passed to that particular foreach?
--ex we have a third argument foreachFns, and it is indexed by the foreachs in the list
--if we see (fns[i].name == foreach_fname) we can see the arguments at that index and pass it in

--Other stuff here

generateBarchartFunction::String
generateBarchartFunction = "function barchart_display(row){}" 

--rows are all the rows, el is the element you want to count occurrences of.
generateCountKeyFunction::String
generateCountKeyFunction = "function countKey(rows, el){ \
    \ count = 0; \n \
    \ \tfor(i =0; i < rows.length; i++){\n \ 
    \ \t\t for(key in rows[i]){ \n\
    \ \t\t\t if(el == key){ \n\
    \ \t\t\t\tcount++ \n\
    \ \t\t\t} \n\
    \ \t\t } \n\
    \ \t} \n\
    \ return count \n \
    \ \n }"


--go through and tab all unique elements of a key
--returns an object
generateDisplayTable::String
generateDisplayTable = "function display_table(rows, key){ \
    \ \n OccurrencesOfVal = new Object() \    
    \ \n\tfor(i =0; i < rows.length; i++){ \ 
    \ \n\t\t string = rows[i][key]\
    \ \n\t\t el = string.split(', ') \ 
    \ \n\t\t\t for(i =0; i < el.length; i++){ \ 
    \ \n\t\t\t\t if(OccurrencesOfVal.hasOwnProperty(el[i])){ \ 
    \ \n\t\t\t\t\t OccurrencesOfVal[el[i]] += 1;}  \
    \ \n\t\t\t\t else{ \
    \ \n\t\t\t\t\t OccurrencesOfVal[el[i]] =  1;}  \
    \ \n\t\t\t\t }\
    \ \n \t\t\t} \
    \ \n return OccurrencesOfVal \
    \ }" 
--iterate over all rows
---- get all values for the key in the row
----look at each element returned from the row
