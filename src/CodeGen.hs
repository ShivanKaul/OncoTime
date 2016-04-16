                    {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen where

import Types
import Lexer
import Data.List
import Parser
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Parsec.String
import Text.Regex
import Debug.Trace


generateSQL :: (Program Annotation)->DBConfig ->(Config Annotation)-> String
generateSQL program@(Program header docs usefilelist groups filt comps) dbconf weedconf =
    do
        let diagnosis = (checkIfDiagnosis filt)
        let query = generateQueries filt dbconf diagnosis
        let displayFunction = generateDisplayFunction comps dbconf weedconf diagnosis
        let eventQueries = generateEventQueries  filt dbconf comps
        --trace (intercalate " ; " (composeEvents filt dbconf)) 
        let toprint = displaySequence comps
        generateScaffoldingJSV2 eventQueries toprint


        -- generateScaffoldingJS query dbDisplayFunction

generatePrettyRowFunction :: String
generatePrettyRowFunction = "function generatePrettyRow(row) {\n\
            \\treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"

explodeSequences :: [SeqField Annotation] -> [[SeqField Annotation]]
explodeSequences seqs =[]
    -- do
        -- [ event1 -> event2 | event3 -> event4] => [[ event1 -> event2 -> event4],[event1 -> event3 -> event4]]

        -- concat (foldl (\acc cur -> case cur of
        --     sequ@(Comma events) -> map (\x -> acc ++ [x]) (handleComma sequ)
        --     sequ@(Bar events) -> map (\x -> acc ++ [x]) (handleBars sequ)) [[]] seqs)

        -- [ event1 -> {event2, event3} -> event4]
        -- [ event1 -> {event2, event3}* -> event4] NOT SUPPORTED

-- handleComma :: SeqField Annotation -> [[SeqField Annotation]]
-- handleComma sequ@(Comma events) =
--     do
--         map (\xs -> map (\x -> Bar [x]) xs) (filter (not . null) (subsequences events))

-- handleBars :: SeqField Annotation -> [[SeqField Annotation]]
-- handleBars sequ@(Bar events) =
--     do
--         map (\x -> [Bar [x]]) (events)

generateEventQueries :: [Filter Annotation] ->DBConfig-> [Computation Annotation] -> [String]
generateEventQueries filt dbconf computations  =
   ["db.query('"++(intercalate " ; " (composeEvents filt dbconf))++"',function(err, rows, fields) {\
            \if (err) throw err;\
             \\nvar flattenedrows = rows.reduce(function(a, b){    \
                \\nreturn a.concat(b);\
             \});\
            \\ndisplay(flattenedrows);\n\
        \});"]

        

displaySequence:: [Computation Annotation]->String
displaySequence computations = 
    do
        foldl' (\prev comp -> case comp of  
            Foreach (ForEachSequence (Var v1 an) seqList) comps _ -> (prev ++ "\nvar "++v1++" = arrangeSequences(rows,"++ (show $ getEventNames seqList)++");\n"++displaySequence comps)
            Print(PrintVar (Var val (Annotation _))) -> (prev ++ "console.log("++val++");")
            _ -> prev) "" computations
getEventNames :: [(SeqField a)] -> [String]
getEventNames seqList =  nub $ concat $ map (\seqfield -> case seqfield of 
    Bar x -> map (\(Event eventname a) -> if eventname=="end" then "end_of_treatment_note_finished" else eventname ) x
    Comma x -> map (\(Event eventname a) -> eventname ) x
    _ -> []
    ) seqList

collectWHEREs :: [Filter Annotation] -> [SeqField Annotation] -> String
collectWHEREs filters events = undefined
collectSELECTs :: [SeqField Annotation] -> String
collectSELECTs events = undefined
collectFROMs :: [SeqField Annotation] -> String
collectFROMs events = undefined

generateDisplayFunction :: [Computation Annotation] ->DBConfig->(Config Annotation)-> Maybe [String] -> String
generateDisplayFunction comps dbconf conf diag =
    do
        let compCodeList = intercalate "\n" (map (genCompCode dbconf conf diag) comps)
        compCodeList

getNameInDatabase :: DBConfig -> String -> String
getNameInDatabase (DBConfig dbconf) name =
    case M.lookup name dbconf of
        Just a -> a
        Nothing -> error $ name ++ " is not a valid name in " ++ show dbconf


genCompCode::DBConfig->Config Annotation->Maybe [String] ->Computation Annotation ->String
genCompCode  dbconf conf diag (Foreach forDef compList _) = (forEachGen forDef dbconf conf diag (generateDisplayFunction compList dbconf conf diag))
genCompCode dbconf conf diag (Table v filtName fieldName) = ""  --left axis is index, right is value name, and then value
genCompCode dbconf conf _ (List v seqLsit) = ""
genCompCode dbconf conf _ (Print paction) = printGen paction dbconf conf
genCompCode dbconf conf _ (Barchart v) =  " // This is a cool barchart. We will use d3"

printGen::PrintAction Annotation->DBConfig -> Config Annotation->String
printGen (PrintVar (Var val (Annotation an))) dbconfmap (Config conf) =
    let
        isLoopable = (M.member (an,True) (conf) )
        printstmt = if isLoopable
            then "\t\ttable.push(generatePrettyRow(" ++ (dbconfmap `getNameInDatabase` an) ++ "));"
            else "/*"++an ++ " has not been implemented yet, sorry!*/"
    in printstmt

printGen(PrintLength var) dbconf  _= "//tables not yet implemented sorry!"
printGen (PrintTimeLine v) dbconf  _= "//Really cool timeline would go here"
printGen (PrintFilters fnList (Var v1 (Annotation an))) ( dbconf) conf= --"// printing filters has not been implemented"
    do
        let dbtablename = (dbconf `getNameInDatabase` an)
        let temp = dbtablename++"_temp"
        "\tvar "++temp++"={};\nfor (var attr in "++dbtablename++") "++temp++"[attr] = '';" ++
            (intercalate "\n " (map (\fname->do
                let fieldname_in_db = (dbconf `getNameInDatabase` fname)
                temp++"."++fieldname_in_db ++ " = " ++ dbtablename++"." ++ fieldname_in_db ) fnList)) ++"\ntable.push(generatePrettyRow("++temp++ "));"  --Take a list of filters, and print the
printGen (PrintElement (Var v1 (Annotation an)) v2) ( dbconf) _ ="/*table printing is unimplemented, sorry!*/"

forEachGen::ForEachDef Annotation->DBConfig->Config Annotation->Maybe [String] ->String->String
forEachGen (ForEachFilter fname (Var v an)) ( dbconfmap) (Config config)  diag stmts  =
    do
        let index_name = "i_" ++ (dbconfmap `getNameInDatabase` fname)
        let loopablename = fname
        let
            forloopbegin = "\tfor (var "++index_name++" = 0; "++index_name ++ "  < rows.length; "++index_name++"++) {\n"
            (Just (FieldMap fieldmap)) =  M.lookup (loopablename, True) config
            fields = M.keys fieldmap
            tableHeaders = case diag of
                Nothing -> (delete "Diagnosis" (nub (map (\x -> dbconfmap `getNameInDatabase` x) fields)))
                _ -> (nub (map (\x -> dbconfmap `getNameInDatabase` x) fields))

            tableLeft = "\tvar table = new Table({\n\
                        \\t\thead: "
            tableCols = case diag of
                Nothing -> ""
                _ -> "\n, colWidths: [20, 50, 10, 20, 20]"
            tableRight = "\n\
                    \\t});\n"

            tableInit = tableLeft ++ (show tableHeaders) ++ tableCols ++ tableRight
            dbtablename = (dbconfmap `getNameInDatabase` loopablename)
            middlestart = "\t\tvar "++ dbtablename ++" = {\n"
            c0 = foldl' (\prev currfield -> prev ++
                            "\t\t    "++
                            currfield++": rows["++index_name++"]."++
                            -- GHETTO AF
                            (if currfield == "Diagnosis" then "Description" else currfield)
                            ++ ",\n") "" tableHeaders
            middleend = "\t\t}\n"
            tablePush = ""
            forloopEnd ="\n\t}\n\treturn table;\n"

        tableInit ++ forloopbegin++middlestart ++c0++middleend++stmts++tablePush++forloopEnd

forEachGen (ForEachTable (Var v1 an1) (Var v2 an2)) db c _ varList = ""
forEachGen (ForEachSequence (Var v1 an) seqList) db c _ varList = "" --print a sequence
forEachGen (ForEachList (Var v1 an1) (Var v2 an2)) db c _ varList = "" -- print a list of sequences



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
                if filtName /= "population"
                    then "/*"++filtName++" filtering has not been implemented yet, sorry! */"
                --then queryString ++ (dbconf M.! filtName) --THIS IS PART OF WHAT BRENDAN DID
                else
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

generateEventQuery :: [Filter Annotation]->DBConfig-> String
generateEventQuery filterlist dbconf@(DBConfig dbconfmap) =
    let
        population = filter(\(Filter filtName fdefList)-> filtName=="population") filterlist
        period = filter(\(Filter filtName fdefList)-> filtName=="period") filterlist
        populationQuery = "  ("++(head $ generateQueries population dbconf Nothing)++") as population "
        eventnamesAndQueries = [("ct_sim_booked","SELECT Appointment.PatientSerNum, \"ct_sim_booked\" as eventname, \n\
            \ Appointment.lastupdated as eventtimestamp from  Appointment  inner join "  ++ populationQuery ++
            "\non population.PatientSerNum = Appointment.PatientSerNum\
            \\n where Appointment.`status` =\"Open\" and Appointment.AliasSerNum = 3 "++  periodF filterlist dbconf)


            ,("ct_sim_completed","SELECT Appointment.PatientSerNum, Appointment.ScheduledStartTime, Appointment.ScheduledEndTime, \"ct_sim_completed\" as eventname, \n\
            \ Appointment.scheduledendtime as eventtimestamp from  Appointment  inner join "  ++ populationQuery ++
            "\non population.PatientSerNum = Appointment.PatientSerNum\
            \ \n where Appointment.`status` =\"Manually Completed\" and Appointment.AliasSerNum = 3 "++  periodF filterlist dbconf)


            ,("patient_arrives","SELECT Appointment.PatientSerNum, PatientLocation.ResourceSer, PatientLocation.CheckedInFlag, \"patient_arrives\" as eventname, \n\
            \ PatientLocation.ArrivalDateTime as eventtimestamp from  PatientLocation inner join Appointment \n\
            \on PatientLocation.AppointmentSerNum = Appointment.AppointmentSerNum inner join\n "  ++ populationQuery ++
            "\non population.PatientSerNum = Appointment.PatientSerNum "++  periodF filterlist dbconf)

            ,("treatment_completed","SELECT Plan.PatientSerNum, \"treatment_completed\" as eventname, \n\
            \ Plan.lastupdated as eventtimestamp from  Plan inner join\n "  ++ populationQuery ++
            "\non population.PatientSerNum = Plan.PatientSerNum where Plan.`status`=\"Completed\" or Plan.`status`=\"CompletedEarly\""++  periodF filterlist dbconf)

            ,("end","SELECT \"end_of_treatment_note_finished\" as eventname, \n\
                \ Document.PatientSerNum, Document.DateOfService as eventtimestamp,\n\
                \Document.DateOfService, Task.CreationDate, \
                \Task.CompletionDate,  Task.DueDateTime,\n\
                \Priority.PriorityCode\n\
                \FROM oncodb.Document inner join oncodb.Task \n\
                \on (oncodb.Document.PatientSerNum = oncodb.Task.PatientSerNum \n\
                \ and  Document.AliasSerNum = 5 and  Task.AliasSerNum = 6) \n\
                \inner join  Priority on  Priority.PrioritySerNum =  Task.PrioritySerNum inner join "  ++ populationQuery ++
            "\non population.PatientSerNum = Document.PatientSerNum where Task.`status`=\"Completed\" " ++  periodF filterlist dbconf) ]
    in intercalate " ;\n " $ snd $ unzip eventnamesAndQueries

eachEvent :: String -> (String,String)
eachEvent eventyouwant = case eventyouwant of
    "ct_sim_booked" -> ("SELECT Appointment.PatientSerNum, \"ct_sim_booked\" as eventname, Appointment.lastupdated as eventtimestamp from  Appointment  inner join "
            ,"on population.PatientSerNum = Appointment.PatientSerNum where Appointment.`status` =\"Open\" and Appointment.AliasSerNum = 3 ")
    "ct_sim_completed" -> ("SELECT Appointment.PatientSerNum, Appointment.ScheduledStartTime, Appointment.ScheduledEndTime, \"ct_sim_completed\" as eventname, \
            \ Appointment.scheduledendtime as eventtimestamp from  Appointment  inner join "
            ,"on population.PatientSerNum = Appointment.PatientSerNum where Appointment.`status` =\"Manually Completed\" and Appointment.AliasSerNum = 3 ")
    "patient_arrives" -> ("SELECT Appointment.PatientSerNum, PatientLocation.ResourceSer, PatientLocation.CheckedInFlag, \"patient_arrives\" as eventname, \
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
    let
        population = filter(\(Filter filtName fdefList)-> filtName=="population") filterlist
        populationQuery = "  ( "++( intercalate " " $ lines(head $ generateQueries population dbconf Nothing))++" ) as population "
    in populationQuery


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

composeEvents :: [Filter Annotation]->DBConfig-> [String]
composeEvents filterlist dbconf@(DBConfig dbconfmap) =
    do
        -- let events = filter(\(Filter filtName fdefList)-> filtName=="events" and filter (\(FieldDef fname fieldvals) -> case fieldvals of
        --                         [fval] -> fval /= GroupWildcard) (fdefList) filterlist)
        let availableEvents = ["patient_arrives","end","ct_sim_completed","ct_sim_booked","treatment_completed"]
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


generateScaffoldingJS :: [String] -> String -> String
generateScaffoldingJS dbQueryList dbDisplayFunction =
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
        let dbQueryLeft = "\t\tdb.query('"
        let dbQueryRight = "', function(err, rows, fields) {\n\
                \\t\t\tif (err) throw err;\n"
        let dbDisplay = "\t\t\tconsole.log(display(rows).toString());\n\
            \\t\t});\n" -- \\t}\n"

        let dbEnd = "\tdb.end();\n\
            \});\n\n"

        let dbDisplayFunctionStart = "function display(rows) {\n"

        let dbDisplayFunctionEnd = "}\n"

        let formatQueryList = map (\x -> dbQueryLeft ++ x ++ dbQueryRight ++
                dbDisplay ++ "\n") dbQueryList
        mysqlReq ++ tableReq ++ config ++ dbConnect ++ (concat formatQueryList) ++
            " \t }\n" ++
            dbEnd ++ generatePrettyRowFunction ++ dbDisplayFunctionStart ++ dbDisplayFunction ++ dbDisplayFunctionEnd


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
\\t    return to_return;\n\
\\t}\n"