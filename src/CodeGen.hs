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

{-
TO DO:
foreach table
for each elements
-}
--look ahead, and gather all computations that have variables. If it is a computation that requires it, store it in this symbol table
--This is for Tables only

--ABLE TO BE ADAPTED
genVarTable::[Computation Annotation]->(M.Map (Var Annotation) [String])-> (M.Map (Var Annotation) [String])
genVarTable [] m = m 
genVarTable ((Foreach fdef compList s):xs) m = case fdef of
    ForEachFilter fn v ->  genVarTable xs m
    ForEachTable v1 v2 -> genVarTable xs m 
    ForEachSequence v seqFieldList -> genVarTable xs m
    ForEachList vmem vlist -> genVarTable xs m
genVarTable ((List v seqFieldList):xs) m = let l = getEventNames seqFieldList in  genVarTable xs (M.insert v l  m) 
genVarTable ((Table v filtName field):xs) m = 
    do
        let l = (\x y -> x:y:[]) filtName field
        genVarTable xs (M.insert v l  m)
genVarTable ((Print p):xs) m = case p of
    PrintVar v -> genVarTable xs m
    PrintLength v -> genVarTable xs m
    PrintFilters filts v -> genVarTable xs m
    PrintElement ind tab -> genVarTable xs m
genVarTable (a:as) b = genVarTable as b

generateSQL :: (Program Annotation)->DBConfig ->(Config Annotation)->JoinConfig-> String
generateSQL program@(Program header docs usefilelist groups filt comps) dbconf weedconf joinconf =
    do
        -- let diagnosis = (checkIfDiagnosis filt)
        -- let query = generateQueries filt dbconf diagnosis
        let varMap = genVarTable comps (M.empty) 
        let queries = generateComputations filt weedconf dbconf joinconf comps varMap
        let scaff = generateScaffoldingJS
        let computationFunctions = generateForEachFunctions 
        let helperFunctions = "\n"++generateSortFunction ++ "\n" ++ generateCountKeyFunction ++ "\n" ++ generateDisplayTable ++ "\n" ++sequencePrinterFunction++ generateBarchartFunction ++ "\n" ++ generateHTMLPage++"\n"++generateDisplayTableLength
        scaff ++ (intercalate "\n" queries)++ "db.end(); \n" ++ computationFunctions ++ helperFunctions 
--
            
generatePrettyRowFunction :: String
generatePrettyRowFunction = "function generatePrettyRow(row) {\n\
            \ \treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"

getNameInDatabase :: DBConfig -> String -> String
getNameInDatabase (DBConfig dbconf) name =
    case M.lookup name dbconf of
        Just a -> a
        Nothing -> error $ name ++ " is not a valid name in " ++ show dbconf

generateEventQueries :: [Filter Annotation] ->DBConfig-> String -> String
generateEventQueries filt dbconf computationString  =
   "db.query('"++(intercalate " ; " (composeEvents filt dbconf))++"',function(err, rows, fields) {\
            \if (err) throw err;\
             \\nvar flattenedrows = rows.reduce(function(a, b){    \
                \\nreturn a.concat(b);\
             \});\n"++  computationString++        "\n});"


displaySequence:: [Filter Annotation] ->DBConfig-> M.Map (Var Annotation) [String]-> Computation Annotation->String
displaySequence filt dbconf varmap comp = 
    do 
        let evqueryfun = generateEventQueries filt dbconf
        case comp of  
            Foreach (ForEachSequence (Var v1 (Annotation an) _) seqList)  [ (Print(PrintVar v ) )] _ -> evqueryfun(
                "\n\tconsole.log( arrangeSequences(flattenedrows,"++ (show $ getEventNames seqList) 
                    ++"));\n")
            Print(PrintVar vlist@(Var val (Annotation "List") _)) -> evqueryfun("console.log("++" arrangeSequences(flattenedrows,"++ (show(varmap M.!  vlist ))++"));\n")
            Print(PrintVar vmem@(Var val (Annotation "member") _)) -> evqueryfun("console.log("++" arrangeSequences(flattenedrows,"++ (show(varmap M.!  vmem ))++"));\n")
            Print(PrintTimeLine (Var val (Annotation _)_) ) -> ( "console.log('timelines have not been implemented yet');\n") 
            Foreach (ForEachList vm@(Var vmem (Annotation anmem) _) vlist) [(Print(PrintVar _ ))] _ -> evqueryfun(
                "\n\tconsole.log( arrangeSequences(flattenedrows,"++ (show(varmap M.!  vlist ))++"));\n")
            _ -> ""

getEventNames :: [(SeqField a)] -> [String]
getEventNames seqList  =  {-if null seqList 
    then availableEvents
    else  -}
        nub $ concat $ map (\seqfield -> case seqfield of 
        Bar x -> map (\(Event eventname a) -> if eventname=="end" then "end_of_treatment_note_finished" else eventname ) x
        Comma x -> map (\(Event eventname a) -> eventname ) x
        _ -> []
        ) seqList
            
generateComputations::[Filter Annotation]->Config Annotation-> DBConfig->JoinConfig-> [Computation Annotation]-> M.Map (Var Annotation) [String]->[String]
generateComputations filterList conf ( dbconfmap@(DBConfig dbconf)) joinconf comps varMap = 
    do
        map (generateComps filterList conf dbconfmap joinconf varMap) comps


--generates each individual query and computation
generateComps::[Filter Annotation]->Config Annotation->DBConfig->JoinConfig->M.Map (Var Annotation) [String]->Computation Annotation->String
generateComps filterList conf (dbconfmap@(DBConfig dbconf)) joinconf  varMap comp = 
    do
        let sequence_statement = displaySequence filterList dbconfmap varMap comp
        if sequence_statement /= ""
        then sequence_statement
        else 
            do 
                let nonperiod = filter(\(Filter filtName fdefList)-> filtName/="period") filterList
                let selectStatement = genFullSQLStatement nonperiod conf dbconfmap joinconf comp varMap
                let generatedCode = (codeGeneration comp dbconfmap varMap) 
                if(selectStatement == "select * from ") then "" 
                else  genFullDBQuery selectStatement generatedCode

codeGeneration::Computation Annotation->DBConfig->M.Map (Var Annotation) [String]->String
codeGeneration comp dbconfmap varMap = 
    do
        case comp of
            (Foreach def compList _) -> case def of
                ForEachFilter filtName v -> filtName++"_functions = ["++ (intercalate ",\n"(map (genForEachFilter dbconfmap) compList)) ++ "]\n"  ++ "foreach_filter(rows, \""++ (dbconfmap `getNameInDatabase` (filtName ++ "_loop")) ++"\", " ++ filtName++ "_functions" ++ ");\n" ++ "\n "
		ForEachTable v1 v2 ->
                    do
                          let supportedOps = filter (== Print (PrintElement v1 v2)) compList
                          let pActions = map (\(Print p) -> p) supportedOps
                          intercalate "\n" $ map (genPrint dbconfmap varMap) pActions
                _ -> ""
            (Table (Var v ann _) filtName fieldName) -> "" 
        -- "= (countKey(rows, \" " ++ (dbconfmap `getNameInDatabase` (fieldName++ "_loop" )) ++ "\" )); \n"
            (List v seqFieldList) -> ""
            (Print p) -> genPrint dbconfmap varMap p
            (Barchart v@(Var va an _)) -> case (M.lookup v varMap) of
                Nothing -> ""
                Just m -> 
			do
				let fname = (dbconfmap `getNameInDatabase` ((map toLower (m!!1))))
				va ++ " = display_table(rows, \"" ++ fname ++"\"); barchart_display(" ++ va++ ", \"" ++ fname ++ "\");"

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


generateQueries::[Filter Annotation]->DBConfig-> Maybe [String] -> [String]
generateQueries filterList ( dbconf@(DBConfig dbconfmap)) diag =
    do
        let columns = case diag of
                Nothing -> "*"
                _ -> "Patient.*, Diagnosis.Description"
        let queryString = "select " ++ columns ++ " from "
        --iterate through filter list
        queryList <- map (\(Filter filtName fdefList) ->
            do 
                let fromQuery = case diag of
                        Nothing -> (dbconf `getNameInDatabase` filtName)
                        _ -> (dbconf `getNameInDatabase` filtName) ++ ", Diagnosis"
                let joinClause = case diag of
                        Nothing -> ""
                        Just diagnoses -> generateWhereClauseForDiag diagnoses ( dbconf )
                let selectQuery = queryString ++ fromQuery
                --get list of fields

                --get list of fields without wildcard
                let filterFieldsWithoutWildcard = filter (\(FieldDef fname fieldvals) -> case fieldvals of
                        [fval] -> fval /= GroupWildcard
                        _ -> fname /= "diagnosis") (fdefList)

                --form the query
                if (length filterFieldsWithoutWildcard) == 0 then selectQuery
                else do
                    let otherClause = (generateWhereClauses ( dbconf) filterFieldsWithoutWildcard filtName)
                    let middle = if((not $ null otherClause) && (not $ null joinClause)) then (" AND ") else " "
                    let whereQuery = " where " ++ joinClause ++  middle ++ otherClause

                    selectQuery ++ whereQuery
                ) filterList
        return queryList


eachEvent :: String -> (String,String)
eachEvent eventyouwant = case eventyouwant of
    "ct_sim_booked" -> ("SELECT Appointment.PatientSerNum, \"ct_sim_booked\" as eventname, Appointment.lastupdated as eventtimestamp from  Appointment  inner join "
            ,"on population.PatientSerNum = Appointment.PatientSerNum where Appointment.`status` =\"Open\" and Appointment.AliasSerNum = 3 ")
    "ct_sim_completed" -> ("SELECT Appointment.PatientSerNum, Appointment.ScheduledStartTime, Appointment.ScheduledEndTime, \"ct_sim_completed\" as eventname, \
            \ Appointment.scheduledendtime as eventtimestamp from  Appointment  inner join "
            ,"on population.PatientSerNum = Appointment.PatientSerNum where Appointment.`status` =\"Manually Completed\" and Appointment.AliasSerNum = 3 ")
    "patient_arrives" -> ("SELECT Appointment.PatientSerNum, PatientLocation.ResourceSer, PatientLocation.CheckedInFlag, \"patient_arrived\" as eventname, \
            \ PatientLocation.ArrivalDateTime as eventtimestamp from  PatientLocation inner join Appointment \
            \on PatientLocation.AppointmentSerNum = Appointment.AppointmentSerNum inner join "
            ," on population.PatientSerNum = Appointment.PatientSerNum ")
    "patient_arrived" -> eachEvent "patient_arrives"
    "patient_appointment" -> eachEvent "patient_arrives"
    "treatment_completed" -> ("SELECT Plan.PatientSerNum, \"treatment_completed\" as eventname,  Plan.lastupdated as eventtimestamp from  Plan inner join "
        , "on population.PatientSerNum = Plan.PatientSerNum where Plan.`status`=\"Completed\" or Plan.`status`=\"CompletedEarly\"")
    "end"->("SELECT \"end_of_treatment_note_finished\" as eventname, Document.PatientSerNum, Document.DateOfService as eventtimestamp,\
                \Document.DateOfService, Task.CreationDate, Task.CompletionDate,  Task.DueDateTime,\
                \Priority.PriorityCode FROM oncodb.Document inner join oncodb.Task on (oncodb.Document.PatientSerNum = oncodb.Task.PatientSerNum \
                \ and  Document.AliasSerNum = 5 and  Task.AliasSerNum = 6) inner join  Priority on  Priority.PrioritySerNum =  Task.PrioritySerNum inner join "
                ,"on population.PatientSerNum = Document.PatientSerNum where (Task.`status`=\"Completed\" OR Task.`status`=\"CompletedEarly\") ")
    "end_of_treatment_note_finished" -> eachEvent "end"

getPopulation :: [Filter Annotation]->DBConfig-> String
getPopulation filterlist dbconf@(DBConfig dbconfmap) =
    do
        let population = filter(\(Filter filtName fdefList)-> filtName=="population" || filtName=="patient" || filtName=="patients") filterlist
        let test =  generateQueries population dbconf Nothing
        if null test then "( SELECT PatientSerNum from Patient) as population "
        else "  ( "++( intercalate " " $ lines(head  test))++" ) as population "



periodF :: [Filter Annotation]->DBConfig-> String
periodF filterlist dbconf@(DBConfig dbconfmap) =
    do
        let period = filter(\(Filter filtName fdefList)-> filtName=="period") filterlist
        if null period
        then ""
        else
            do
                let (Filter _ fdefList) = head period
                let filterFieldsWithoutWildcard = filter (\(FieldDef fname fieldvals) -> case fieldvals of
                                [fval] -> fval /= GroupWildcard) (fdefList)
                if (length filterFieldsWithoutWildcard) == 0 then ""
                else "  having " ++(generateWhereClauses ( dbconf) filterFieldsWithoutWildcard "period")


availableEvents = ["patient_arrives","end","ct_sim_completed","ct_sim_booked","treatment_completed"]


composeEvents :: [Filter Annotation]->DBConfig-> [String]
composeEvents filterlist dbconf@(DBConfig dbconfmap) =
    do
        -- let events = filter(\(Filter filtName fdefList)-> filtName=="events" and filter (\(FieldDef fname fieldvals) -> case fieldvals of
        --                         [fval] -> fval /= GroupWildcard) (fdefList) filterlist)
        -- let  allUSed = null events
        let populationQuery = getPopulation filterlist dbconf
        let periods = periodF filterlist dbconf
        map (\ev -> let (selectClause, whereClause) = eachEvent ev  
                    in selectClause ++ populationQuery ++ whereClause ++ periods) availableEvents


generateWhereClauseForDiag :: [String] -> DBConfig -> String
generateWhereClauseForDiag diagnoses ( dbconfmap) =
    do
        let prefixDiags = "(Patient.PatientSerNum = Diagnosis.PatientSerNum) AND ("
        let whereDiags = map (\diagnosis -> "Diagnosis.DiagnosisCode like " ++
                "\"" ++ (dbconfmap `getNameInDatabase` diagnosis) ++ "%\"") diagnoses
        prefixDiags ++ (intercalate " OR " whereDiags) ++ ") "


generateWhereClauses :: DBConfig->[FieldDef Annotation] -> String -> String
generateWhereClauses ( dbconfmap) fielddefs filtername =
        intercalate " AND " (map (\(FieldDef fieldname fvals) ->
            let
                tname = if filtername == "period" then (dbconfmap `getNameInDatabase` fieldname)
                    else (dbconfmap `getNameInDatabase` filtername) ++ "." ++ (dbconfmap `getNameInDatabase` fieldname)
            in " ( "
                ++  (generateFieldValsForWhere fvals tname) ++ " )" )  fielddefs)


generateFieldValsForWhere :: [FieldVal Annotation] -> String -> String
generateFieldValsForWhere fvals fname =
    do
        let expanded = intercalate " OR "( map (\ fval -> case fval of
            -- TODO: Handle multiple for both of these by having commas
                GroupValString str _ -> (fname) ++ " like \"" ++ str ++ "%\" "
                GroupRange (SingleInt i _) ->  fname ++ " = " ++ (show i) ++ " "
                GroupRange (Before i _) -> fname ++ " < " ++ (show i) ++ "  "
                GroupRange (After i _) ->  fname ++ " > " ++ (show i) ++ "  "
                GroupRange (Between i1 i2 _) ->  " ( "++fname ++ " > " ++ (show i1) ++
                        " AND " ++ fname ++ " < " ++ (show i2) ++ ")  "
                (GroupDate dd mm yy _) ->  fname ++"=\"" ++ (show dd) ++"-"++ (show mm) ++"-"++ (show yy) ++ "\"  "
                )  fvals)
        expanded

getQueryElements::DBConfig->M.Map (Var Annotation) [String]->Computation Annotation->[String]
getQueryElements dbconf varMap comp = 
    do
        case comp of
            (Barchart v) -> case M.lookup v varMap of
                    Nothing->[]
                    Just l -> l
            (Foreach def compList _)->
                case def of
                    (ForEachFilter filtName v) -> (map toLower filtName):(accumulateForEach compList dbconf)
                    _ -> []
             
            (Print p) -> case p of
                PrintVar v -> case M.lookup v varMap of
                    Nothing -> []
                    Just l -> l	
		PrintElement v1 v2 -> case M.lookup v1 varMap of
		    Nothing -> []
                    Just l -> l
            _ -> []

genFullSQLStatement::[Filter Annotation]->Config Annotation->DBConfig->JoinConfig->Computation Annotation->M.Map (Var Annotation) [String]->String
genFullSQLStatement filterList conf dbconfmap@(DBConfig dbmap) joinconf comp varMap = 
    do
        let listOfQueryElements = getQueryElements dbconfmap varMap comp
        --let st = generateScaffoldingJS query (getQueryElements comps) dbconf
        --let st = generateScaffoldingJS query (getQueryElements comps) dbconf
        --separate the fields from the filters
        let fieldNameList = getFieldNameList filterList listOfQueryElements dbconfmap
        --separate the filters from the filterList
        let filterNameList = getFilterNameList filterList listOfQueryElements dbconfmap
        --get filters that are loopable
        let usedFilterNameList = filter (\x-> M.member (x++ "SQL") dbmap) filterNameList
        --get fields that are loopable
        let usedFieldNameList = filter (\x-> M.member (x++ "SQL") dbmap) fieldNameList
        --get select from
        let selectStmt = genSelectStatements dbconfmap joinconf usedFilterNameList usedFieldNameList 
        --get Joins
        let joinStmt = genJoinStatements dbconfmap joinconf usedFilterNameList usedFieldNameList
        --get wheres
        let whereStmt = genWhereStatements filterList dbconfmap joinconf usedFilterNameList usedFieldNameList

        selectStmt ++  joinStmt ++ whereStmt
        --show listOfQueryElements ++ show fieldNameList ++ show usedFieldNameList ++ show usedFilterNameList
--FINISH THE THIS BY 9

--get all the filterNames
--go through the fitler list.
getFilterNameList::[Filter Annotation]->[String]->DBConfig->[FilterName]
getFilterNameList filtList queryElements dbconf = 
    do
        let listOfRealNames  = map (getNameInDatabase dbconf) queryElements 
        filter (\filtName-> filtName `elem` queryElements  || (dbconf `getNameInDatabase` filtName) `elem` listOfRealNames ) (getFilterNames filtList)

--get all the fieldnames 
getFieldNameList::[Filter Annotation]->[String]->DBConfig ->[FieldName]
getFieldNameList filtList queryElements dbconf@(DBConfig db) = 
    do
        let listOfRealNames  = map (getNameInDatabase dbconf) queryElements 
        let includedList = filter (\filtName-> filtName `elem` queryElements  || ((dbconf `getNameInDatabase` filtName) `elem` listOfRealNames ) || ((M.member (filtName ++ "_field")db))) (getFieldNames filtList)
        includedList

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
             True -> case (length fieldNameList) > 0 of
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
             False -> dbHead
        joinStatement
genJoinStatements db@(DBConfig dbconf) (JoinConfig jointo joinableList) (filterNameHead:filterNameList) []  =  --case where one exist
    do
        let dbHead = (\filt-> (db `getNameInDatabase` filt)) filterNameHead
        let dbFields = map  (\filt-> (db `getNameInDatabase` filt)) filterNameList
        let joinStatement = case (length (filterNameHead:filterNameList)) > 1 of
             True -> 
                 do
                    dbHead ++ concat ( map (\field ->" JOIN " ++ field ++ " ON " ++  field ++ "."++ jointo ++ " = " ++ dbHead ++ "."++jointo ++ " "   ) dbFields )
             False -> dbHead
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
fieldValToWhere sqlName (GroupValString str an ) = if "Description" `isInfixOf` sqlName   
    then "( " ++ sqlName  ++ " like \"%" ++ str ++ "%\"" ++ " )" 
    else "( " ++ sqlName  ++ " like \"" ++ str ++ "%\"" ++ " )" 
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
            \\t\t\tif (err) throw err;\n else{"
        let dbQueryEnd = "\n} \n });" --This goes around each one?
        dbQueryLeft ++ selectStatement ++ dbQueryRight ++ genCode ++ dbQueryEnd
--db.end() });

genForEachFilter::DBConfig->Computation Annotation->String
genForEachFilter dbconfmap (Foreach def compList _) = 
    case def of
        ForEachFilter filtName v -> "\t(function(" ++ filtName ++"_row){\n\t" ++ filtName++"_fns = [\n" ++  (intercalate ",\t\n" (map (genForEachFilter dbconfmap) compList)) ++"\n\t ]\n" ++ "foreach_filter("++filtName++"_row,\""++(dbconfmap `getNameInDatabase` (filtName ++ "_loop")) ++"\", " ++ filtName ++"_fns) })"
        ForEachTable v v2 -> "NOT SUPPORTED"
        ForEachSequence v seqfield ->"NOT SUPPORTED"
        _ -> "NOT SUPPORTED"
genForEachFilter dbconfmap (Table (Var v an _)  fil fie) = "" --"function("++fil++"_row){" ++ v ++ "= display_table("++fil++"_row, \"" ++ (dbconfmap `getNameInDatabase` fie)  ++ "\")}\n"
genForEachFilter db (Print p ) = genPrintInForeach p db
genForEachFilter _ (Barchart v) = "" 
genForEachFilter _ (List v seqList) = "" 

--How do I generate all these print statements?
--use the var given!!!

genPrint::DBConfig->M.Map (Var Annotation) [String]->PrintAction Annotation ->String
genPrint db varMap (PrintVar var@(Var v (Annotation "Table") _)) = case (M.lookup var varMap) of
    Nothing -> ""
    Just m -> v ++ "= display_table(rows, \"" ++ (db `getNameInDatabase` ((map toLower (m!!1))++"_table"))++"\", false); console.log("++v++")"
genPrint db varMap (PrintLength var@(Var tab a _ )) =  case (M.lookup var varMap) of
    Nothing -> ""
    Just m -> "display_table_length(rows, \"" ++ (db `getNameInDatabase` ((map toLower (m!!1))++"_table"))++"\", true);"
genPrint db _ (PrintFilters filts v@(Var varName an _)) = "function PrintFilters(row){" ++ (genPrintFilterString v filts db) ++ "console.log(" ++ varName ++")}"--like print id,sex of. --needs to be anonymous, otherwise I can't do it 
genPrint db varMap (PrintElement var@(Var tab a _ ) (Var index an _)) = case (M.lookup var varMap) of
    Nothing -> ""
    Just m -> "display_table(rows, \"" ++ (db `getNameInDatabase` ((map toLower (m!!1))++"_table"))++"\", true);"



genPrintInForeach::PrintAction Annotation ->DBConfig->String
genPrintInForeach (PrintVar (Var v (Annotation an) _)) db = "function PrintVar("++ v ++"){"++v++".forEach(function(entry){console.log(\"" ++ an ++ ": \" + entry[\"" ++ (db `getNameInDatabase`((map toLower an) ++ "_loop"))++ "\"])}\n)}" --vanilla case

genPrintInForeach (PrintLength (Var v (Annotation an) _)) db =  "function CountVar("++v++"){console.log(countKey(v, "++ (db `getNameInDatabase` an) ++")) });"--count???

genPrintInForeach (PrintFilters filts v@(Var varName an _)) db = "function PrintFilters(row){" ++ (genPrintFilterString v filts db) ++ "console.log(" ++ varName ++")}"--like print id,sex of. --needs to be anonymous, otherwise I can't do it 

genPrintInForeach (PrintElement (Var index a _) (Var tab an _)) _ = "function PrintElement("++ index ++ ", "++ tab ++"){console.log("++ tab ++"["++index++ "])}"

genPrintFilterString::(Var Annotation)->[FilterName]->DBConfig->String
genPrintFilterString (Var v an _) filtList  dbconf= v ++ " = {" ++ (intercalate ",\n" (map (\filt -> filt ++ " : row." ++ (dbconf `getNameInDatabase` filt)) filtList )) ++ "\n};"

accumulateForEach::[Computation Annotation]->DBConfig->[String]
accumulateForEach [] _ = []
accumulateForEach ((Foreach (ForEachFilter fn v) compList  spos):xs) db = fn : (accumulateForEach xs db) ++ (accumulateForEach compList db) 
accumulateForEach ((Foreach (ForEachTable (Var v1 a1 _) (Var v2 a2 _)) compList  spos):xs) db = (db `getNameInDatabase` v2) : (accumulateForEach xs db) ++ (accumulateForEach compList db) 
accumulateForEach (x:xs) db= (accumulateForEach xs db)  

isForEachFilter::(Computation Annotation) -> Bool
isForEachFilter (Foreach (ForEachFilter _ _) _ _ ) =  True
isForEachFilter _ = False

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
                \\tmultipleStatements: true,\n\
                \\tport: 33306\n\
            \});\n"
        let plotly = "var username = 'dragonarmy' \n \
		\ \t var api_key = 'fokgu2ai5j' \n \
		\ \t require('plotly')(username, api_key);\n \
                \ \t require('fs'); \n"

        mysqlReq ++ tableReq ++ config ++ plotly


generateDisplayPrettyFunction::String
generateDisplayPrettyFunction = "function generatePrettyRow(row) {\n \treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"

--generates all necessary print functions
generateDisplayPrintFunction::String
generateDisplayPrintFunction = "function print_var(row) {\n \t console.log(row)\n\
        \}\n\n"



generateScaffoldingJSV2 :: [String] -> String -> String
generateScaffoldingJSV2 dbQueryList dbDisplayFunction =
    do
        let mysqlReq = "var mysql = require('mysql');\n"
        let tableReq = "var Table = require('cli-table');\n"
        let config = "var db = mysql.createConnection({\n\
                \\thost: 'localhost',\n\
                \\tuser: '520student',\n\
                \\tpassword: 'comp520',\n\
                \\tdatabase: 'oncodb',\n\
                \\tmultipleStatements: true,\n\
                \\tport: 33306\n\
            \});\n"
        let dbConnect = "db.connect(function(err) {\n\
                \\tif (err) console.log(err);\n\
                \\telse {\n"

        let dbEnd = "\t}\n\tdb.end();\n\
            \});\n\n"

        let dbDisplayFunctionStart = "function display(rows) {\n"
        let dbDisplayFunctionEnd = "}\n"

        mysqlReq ++ tableReq ++ config ++ dbConnect ++ (concat dbQueryList) ++ dbEnd ++
            generatePrettyRowFunction ++ dbDisplayFunctionStart ++ dbDisplayFunction ++
            dbDisplayFunctionEnd ++sequencePrinterFunction



        -- let formatQueryList = map (\x -> dbQueryLeft ++ x ++ dbQueryRight ++
        --         dbDisplay ++ "\n") dbQueryList
        -- mysqlReq ++ tableReq ++ config ++ dbConnect ++ (concat formatQueryList) ++
        --     " \t }\n" ++
        --     dbEnd ++ generatePrettyRowFunction ++ dbDisplayFunctionStart ++ dbDisplayFunction ++ dbDisplayFunctionEnd



sequencePrinterFunction :: String
sequencePrinterFunction = 
    "\tfunction arrangeSequences (flattenedrows,current_sequence){\n\
    \\tvar events_by_patient={/*1234:{ev:[{1234}],ev1:[{1234}],ev2:[{1234}],*/};\n\
    \\tfor (var i_seq = 0; i_seq  < flattenedrows.length; i_seq++) {\n\
    \\t    var current_pat = flattenedrows[i_seq].PatientSerNum;\n\
    \\t    var patientevents = events_by_patient[current_pat];\n\
    \\t    if (patientevents==undefined){\n\
    \\t        patientevents={};\n\
    \\t        patientevents[flattenedrows[i_seq].eventname] = flattenedrows[i_seq];\n\
    \\t        events_by_patient[current_pat]=patientevents;\n\
    \\t        }\n\
    \\t    else{\n\
    \\t        var current_events_of_type = patientevents[flattenedrows[i_seq].eventname];\n\
    \\t        if (current_events_of_type == undefined){\n\
    \\t            current_events_of_type=flattenedrows[i_seq];\n\
    \\t            patientevents[flattenedrows[i_seq].eventname] = current_events_of_type ;\n\
    \\t        } else{\n\
    \\t            if(flattenedrows[i_seq].eventtimestamp<current_events_of_type.eventtimestamp){\n\
    \\t                //current_events_of_type.pop();\n\
    \\t                //current_events_of_type.push(flattenedrows[i_seq]);\n\
    \\t                patientevents[flattenedrows[i_seq].eventname]=flattenedrows[i_seq];\n\
    \\t            }\n\
    \\t            //current_events_of_type.push(flattenedrows[i_seq]);\n\
    \\t        }\n\
    \\t    }\n\
    \\t}\n\
    \\treturn filterSequences(events_by_patient,current_sequence);\n\
\\t}\n\n\
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
\\tfunction filterSequences(events_by_patient,current_sequence){\n\
\\t    var to_return ={};\n\
\\t    for (var patientSerNum in events_by_patient){\n\
\\t        var to_save = {};\n\
\\t        var passes_all_checks = true;\n\
\\t        for(var i_seq = 0; i_seq  < current_sequence.length; i_seq++) {\n\
\\t            var current_event = current_sequence[i_seq];\n\
\\t            if( events_by_patient[patientSerNum][current_event]==undefined){\n\
\\t                passes_all_checks = false;\n\
\\t                break;\n\
\\t            }else{\n\
\\t                to_save[current_event]=events_by_patient[patientSerNum][current_event];\n\
\\t            }\n\
\\t        }\n\
\\t        if (passes_all_checks){\n\
\\t            to_return[patientSerNum] = to_save;\n\
\\t        }\n\
\\t    }\n\
\\t    return to_return;\n }\n"

generateSortFunction::String
generateSortFunction =  "function sortObj(list, key) {  \n \
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

generateForEachFunctions::String
generateForEachFunctions = "function foreach_filter(rows, key, functions){ \n \ 
\ var sortedRows = sortObj(rows, key)\n \
\ var arrOf = new Array()\n \
\ var prev_index = 0;\n \
\ for(var i = 0; i < rows.length; i++){\n \
\ if(arrOf.length == 0){ \n \
\ arrOf[prev_index] = new Array()\n \
\ arrOf[prev_index].push(sortedRows[i])\n \
\ } \n \
\ else if(arrOf[prev_index][0][key] == sortedRows[i][key] ){\n \
\ arrOf[prev_index].push(sortedRows[i]) \n \
\ } else { \n \
\ prev_index ++\n  \
\ arrOf[prev_index] = new Array() \n \
\ arrOf[prev_index].push(sortedRows[i]) \n \
\ } \n \
\ }\n \
\ for(var j = 0; j < arrOf.length; j++){\n \
\ for(var k = 0; k < functions.length; k++){\n \
\ functions[k](arrOf[j]);\n \
\ }\n \
\ }\n \
\  }"

--code check for an additional argument, it being the arguments that are passed to that particular foreach?
--ex we have a third argument foreachFns, and it is indexed by the foreachs in the list
--if we see (fns[i].name == foreach_fname) we can see the arguments at that index and pass it in

--Other stuff here
{-
generateBarchartFunction::String
generateBarchartFunction = "function barchart_display(obj){ \n\

\ var labels = Object.keys(obj) \n \
\ //collect y data \n\
\ var vals = Object.values(obj) \n \
\ var data = [ \n \
\  \t{\n \
\  \t\t x: labels, \n \
\  \t\t y: vals, \n \
\  \t\t  type: 'bar'\n \
\  \t}\n\
\ ];\n \
\ var graphOptions = {filename: 'basic-bar', fileopt: 'overwrite'};\n\
\plotly.plot(data, graphOptions, function (err, msg) { \n\
\    console.log(msg); \n\
\}); }"
-}

--object.values function retrieved from http://stackoverflow.com/questions/14791917/object-values-in-jquery
generateBarchartFunction::String
generateBarchartFunction = "function barchart_display(obj,fname){ \n\
\  //collect x data \n\
\ Object.values = function(object) { \n \
\  var values = []; \n \
\  for(var property in object) { \n \
\    values.push(object[property]);\n \
\  } \n \
\  return values; \n\
\} \n \
\ var labels = Object.keys(obj) \n \
\ //collect y data \n\
\ var vals = Object.values(obj) \n \
\ var js = 'var data = [{x: [' + labels + '],y: ['+ vals + '], type: \"bar\"}]; Plotly.newPlot(\"myDiv\", data);' \n \
\ create_html(js, fname); \n \
\}; "



--rows are all the rows, el is the element you want to count occurrences of.
generateCountKeyFunction::String
generateCountKeyFunction = "function countKey(rows, el){ \
    \ var count = 0; \n \
    \ \tfor(var i =0; i < rows.length; i++){\n \ 
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
generateDisplayTable = "function display_table(rows, key, printRow){\n \
    \ \n OccurrencesOfVal = new Object()\n \    
    \ \n\tfor(i =0; i < rows.length; i++){\n \ 
    \ \n\t\t string = rows[i][key] \n\
    \ \n\t\t\t\t if(OccurrencesOfVal.hasOwnProperty(string)){\n \ 
    \ \n\t\t\t\t\t OccurrencesOfVal[string] += 1; \n \
    \ \n\t\t\t\t }else{\n \
    \ \n\t\t\t\t\t OccurrencesOfVal[string] =  1; \n \
    \ \n\t\t\t\t }\n\
    \ \n\t}\n\
    \ \nif(printRow){console.log(OccurrencesOfVal)}\n\
    \ \n return OccurrencesOfVal\n \
    \ }"
generateDisplayTableLength::String
generateDisplayTableLength = "function display_table_length(rows, key){\n \
    \ \n var len = 0;\n \  
    \ \n OccurrencesOfVal = new Object()\n \    
    \ \n\tfor(i =0; i < rows.length; i++){\n \ 
    \ \n\t\t string = rows[i][key] \n\
    \ \n\t\t\t\t if(OccurrencesOfVal.hasOwnProperty(string)){\n \ 
    \ \n\t\t\t\t\t OccurrencesOfVal[string] += 1; \n \
    \ \n\t\t\t\t }else{\n \
    \ \n\t\t\t\t\t len+=1;\
    \ \n\t\t\t\t\t OccurrencesOfVal[string] =  1; \n \
    \ \n\t\t\t\t }\n\
    \ \n\t }\n\
    \ \n console.log(\"Table of\",key,\"has length\", len);\n \
    \ }"
--iterate over all rows
---- get all values for the key in the row
----look at each element returned from the row

generateHTMLPage::String
generateHTMLPage = "function create_html(js, fname){\n \
\var html = '<head> <!-- Plotly.js -->'  \n \ 
\ + '<script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>' \n \
\ + '</head>' \n \
\+  '<body> <p> ' + fname + ' Graph </p>' \n \
\+  '<div id=\"myDiv\" style=\"width: 480px; height: 400px;\"><!-- Plotly chart will be drawn inside this DIV --></div>' \n \
\+ '<script>' + js + '</script>'  \n \
\+ '</body>'; \n \
\ var write = require(\"fs\").writeFile  \n \
\write(__dirname + '/' + fname +'.html', html , function(err) { \n \
\    if(err) { \n \
\        return console.log(err);\n \
\    } \n \
\    console.log(\"The file was saved!\"); \n \
\});}"
