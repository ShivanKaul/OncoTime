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
        let query = generateComputations filt weedconf dbconf joinconf comps
        let st = generateScaffoldingJS query 
        st
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


-- map over all computations
-- fold over a list of computations, and apply this particular function, which will find the right code to gen. my responsibility is the foreach

--For each, what I want to do is generate teh db query. from db.query(... to the end of the function

--go overeach computatio
----check to see what query it needs, and then generate
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
                        ForEachFilter filtName v -> filtName++"_fns = [\n" ++  (intercalate "," (map (genForEachFilter dbconfmap)compList )) ++"\n ]\n" ++ "foreach_fname(rows," ++ filtName ++ "_fns);" ++ "});\n }"
                        _ -> ""
                (Table v filtName fieldName) -> ""
                (List v seqFieldList) -> ""
                (Print p) -> ""
                (Barchart v) -> ""


        let code = genFullDBQuery selectStatement generatedCode
        code

--select Patient.*, PatientDoctor.*, Diagnosis.* from Patient JOIN PatientDoctor On PatientDoctor.PatientSerNum = Patient.PatientSerNum JOIN Diagnosis on Patient.PatientSerNum = Diagnosis.PatientSerNum limit 100;


genFullSQLStatement::[Filter Annotation]->Config Annotation->DBConfig->JoinConfig->Computation Annotation->String
genFullSQLStatement filterList conf dbconfmap joinconf comp = 
    do
        let filterNameList= case comp of
             (Foreach def compList _)->
                case def of
                    (ForEachFilter filtName v) -> (map toLower filtName):(accumulateForEach compList )
                    _ -> []
             (Table v filtName fieldName) -> filtName:[]
             _ -> []

        --get select from
        let selectStmt = genSelectStatements dbconfmap joinconf filterNameList 

        --get Joins
        let joinStmt = genJoinStatements dbconfmap joinconf filterNameList 

        --get wheres
        let whereStmt = genWhereStatements filterList dbconfmap joinconf filterNameList 
        
        --concat
        selectStmt ++ joinStmt ++ whereStmt

genSelectStatements::DBConfig->JoinConfig->[String]->String
genSelectStatements db@(DBConfig dbconf) joinconf filterNameList = 
    do
        let selectStmt = case (length filterNameList) > 0 of
             True -> "select " ++ (intercalate ", " (map (\filt-> (db `getNameInDatabase` filt) ++ ".*") filterNameList)  ) ++ " from "
             False -> "select * from "
       
        selectStmt


--JOIN PatientDoctor On PatientDoctor.PatientSerNum = Patient.PatientSerNum JOIN Diagnosis on Patient.PatientSerNum = Diagnosis.PatientSerNum limit 100;

genJoinStatements::DBConfig->JoinConfig->[String]->String
genJoinStatements db@(DBConfig dbconf) (JoinConfig jointo joinableList) (filterNameHead:filterNameList) = 
    do
        let dbHead = (\filt-> (db `getNameInDatabase` filt)) filterNameHead
        let dbFields = map  (\filt-> (db `getNameInDatabase` filt)) filterNameList
        let joinStatement = case (length (filterNameHead:filterNameList)) > 1 of
             True -> 
                 do
                    concat ( map (\field -> "JOIN " ++ field ++ " ON " ++  field ++ "."++ jointo ++ " = " ++ dbHead ++ "."++jointo ++ " "   ) dbFields )
             False -> ""
        joinStatement
        
genWhereStatements::[Filter Annotation]->DBConfig->JoinConfig->[String]->String
genWhereStatements filtList db@(DBConfig dbconf) jc@(JoinConfig jointo joinableList) filterNameList = 
    do
        --if a filter, get all
        let filterWhere = map (genWhereFilter filtList db jc) filterNameList
        -- if a field, 
        let fieldWhere = map (genWhereField filtList db jc) filterNameList

        case (length filterWhere > 0) of
            True -> case (length fieldWhere > 0) of
                True -> " where " ++ (intercalate " AND " filterWhere) ++ " AND " ++ (intercalate " AND " fieldWhere)
                False -> " where " ++ (intercalate " AND " filterWhere)
            False ->case (length fieldWhere > 0) of
                True-> " where " ++ (intercalate " AND " fieldWhere)
                False-> "" 

--TODOOOOO
genWhereField::[Filter Annotation]->DBConfig->JoinConfig->String->String
genWhereField filtList dbconf joinconf fieldName = 
    do 
        let sqlName = (dbconf `getNameInDatabase` fieldName)
        
        let listOfConditions = []
        "(" ++ intercalate " OR " listOfConditions ++ ")"


genWhereFilter::[Filter Annotation]->DBConfig->JoinConfig->String->String
genWhereFilter filtList db@(DBConfig dbconf) joinconf filterName =
    do
        let fields = (find (\(Filter filtName fdef) -> filterName == filtName ) filtList)
        let sqlName = (db `getNameInDatabase` filterName)
        let listOfConditions = filterToWhere fields db sqlName
        "(" ++ intercalate " OR " listOfConditions ++ ")"


filterToWhere::Filter Annotation->DBConfig->String->[String]
filterToWhere (Filter filt fdef) db@(DBConfig dbconf) sqlName = 
    do
        map (fieldDefToWhere sqlName db) fdef


fieldDefToWhere::String->DBConfig->FieldDef Annotation->String
fieldDefToWhere sqlName db@(DBConfig dbconf) (FieldDef fname fvals) = 
    do
        (intercalate " OR " (map (fieldValToWhere (sqlName ++ "." ++ (db `getNameInDatabase` fname))) fvals))
        

fieldValToWhere::String->FieldVal Annotation->String
fieldValToWhere sqlName (GroupValString str an ) = sqlName  ++ "like \"" ++ str ++ "%\""
fieldValToWhere sqlName (GroupRange (Before i a )) = sqlName ++ " <" ++ show i
fieldValToWhere sqlName (GroupRange (After i a )) = sqlName ++ "> " ++ show i
fieldValToWhere sqlName (GroupRange (Between i j a )) = sqlName ++ " > " ++ show i ++ " AND " ++ sqlName ++ "< " ++ show j --HOW?
fieldValToWhere sqlName (GroupRange (SingleInt i a)) = "= " ++ show i
fieldValToWhere sqlName (GroupDate dd mm yy an) = "HOW?" -- HOW?
fieldValToWhere _ _ = "" 




genFullDBQuery::String->String->String
genFullDBQuery selectStatement  genCode = 
    do
        let dbQueryLeft = "\t\tdb.query('"
        let dbQueryRight = "', function(err, rows, fields) {\n\
            \\t\t\tif (err) throw err;\n"
        let dbQueryEnd = "\n});" --This goes around each one?
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
 --       ForEachFilter filtName v ->  (" function(rows){" ++ filtName++"_fns = [\n" ++  (intercalate "," (map (genForEachFilter dbconfmap) compList)) ++"\n ]\n" ++ "foreach_fname(rows," ++ filtName ++ "_fns);" ++ "}\n") 
        ForEachFilter filtName v -> "(function(rows){\n\t" ++ filtName++"_fns = [\n" ++  (intercalate ",\n" (map (genForEachFilter dbconfmap) compList)) ++"\n ]\n" ++ " for(i =0; i < rows.length; i++){\n\ 
    \ \t for(j =0; j < "++ filtName ++ "_fns.length; j++){ \n\
    \ \t\t fns[j](rows[i]); \
    \ \n \t } \n \
    \ } \n}\n })"
        _ -> "NOT SUPPORTED"
genForEachFilter _ (Table v fil fie) = ""
genForEachFilter db (Print p) = genPrintInForeach p db
genForEachFilter _ (Barchart v) = ""
genForEachFilter _ (List v seqList) = ""


--How do I generate all these print statements?
--use the var given!!!
genPrintInForeach::PrintAction Annotation ->DBConfig->String
genPrintInForeach (PrintVar (Var v an)) _ = "function PrintVar("++ v ++"){console.log("++ v ++")}"--vanilla case
genPrintInForeach (PrintLength v) _ = ""--count???
genPrintInForeach (PrintFilters filts v@(Var varName an)) db = "function PrintFilters(row){" ++ (genPrintFilterString v filts db) ++ "console.log(" ++ varName ++")}"--like print id,sex of. --needs to be anonymous, otherwise I can't do it 
genPrintInForeach (PrintElement (Var index a) (Var tab an)) _ = "function PrintElement("++ index ++ ", "++ tab ++"){console.log("++ tab ++"["++index++ "])}"

genPrintFilterString::(Var Annotation)->[FilterName]->DBConfig->String
genPrintFilterString (Var v an) filtList (DBConfig dbconf)= v ++ " = {" ++ (intercalate ",\n" (map (\filt -> filt ++ " : row." ++ (dbconf M.! filt)) filtList )) ++ "\n};"


--A FUNCTION HERE TO JOIN EVERYTHING THAT CAN BE LOOPED OVER???

--problems: repeated stuff
--hard coding
--no error handlgin
--A table of joins would be nice. 

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
            

--gets names of all the filters used
accumulateForEach::[Computation Annotation]->[String]
accumulateForEach compList= 
    do
        let filts = filter (isForEachFilter) compList
        let filtNames = map (\(Foreach (ForEachFilter fname _) _ _)  -> (map toLower fname)) filts
        filtNames 


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


generateScaffoldingJS :: [String] -> String -- -> String
generateScaffoldingJS dbQueryList = --funcs=  -- dbDisplayFunction =
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
        let dbEnd = "\t}\tdb.end();\n \n});\n "

        let dbDisplayFunctionStart = "function display(rows) {\n"
        let dbDisplayFunctionEnd = "}\n"

--        let formatQueryList = map (\x -> dbQueryLeft ++ x ++ dbQueryRight ++
  --              dbDisplay ++ "\n") dbQueryList


        let formatQueryList = map (\x -> x ++ "\n") dbQueryList
        
        mysqlReq ++ tableReq ++ config ++ dbConnect ++ (concat formatQueryList) ++ 
            dbEnd ++ generateDisplayPrintFunction ++ "\n" ++ generateBarchartFunction ++ "\n" ++ generateForEachFunctions ++ "\n" ++ generateCountKeyFunction ++"\n" ++ generateDisplayTable
            -- generatePrettyRowFunction ++ dbDisplayFunctionStart ++ dbDisplayFunction ++ dbDisplayFunctionEnd
            --
generateDisplayPrettyFunction::String
generateDisplayPrettyFunction = "function generatePrettyRow(row) {\n \treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"

--generates all necessary print functions
generateDisplayPrintFunction::String
generateDisplayPrintFunction = "function print_var(row) {\n \t console.log(row)\n\
        \}\n\n"


generateForEachFunctions::String
generateForEachFunctions = "function foreach_fname(rows, fns){ \n\
    \ for(i =0; i < rows.length; i++){\n\ 
    \ \t for(j =0; j < fns.length; j++){ \n\
    \ \t\t fns[j](rows[i]); \
    \ \t \n } \n \
    \ } \n}\n"

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
