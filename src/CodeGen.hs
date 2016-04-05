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
        let displayFunction = generateDisplayFunction comps  dbconf weedconf diagnosis
        generateScaffoldingJS query displayFunction

generatePrettyRowFunction :: String
generatePrettyRowFunction = "function generatePrettyRow(row) {\n\
            \\treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"


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
        dbName = if isLoopable
            then (dbconfmap `getNameInDatabase` an)
            else "'"++an ++ " has not been implemented yet, sorry!'"
    in "\t console.log(" ++ dbName ++ ");"

printGen(PrintLength var) dbconf  _= "//tables not yet implemented sorry!"
printGen (PrintTimeLine v) dbconf  _= "//Really cool timeline would go here"
printGen (PrintFilters fnList v) ( dbconf) _=
    do
        "\t console.log(" ++ (intercalate ", " (map (\fname->
            do
                let dbName = (dbconf `getNameInDatabase` fname)
                fname ++ ": " ++ "rows[" ++ "i_"++dbName++ "]." ++ dbName ) fnList)) ++ ");"  --Take a list of filters, and print the
printGen (PrintElement (Var v1 (Annotation an)) v2) ( dbconf) _ =
    do
        let dbName = (dbconf `getNameInDatabase` an)
        "\t console.log(\"" ++ an ++ "\": " ++ "rows[" ++ "i_"++dbName++ "]." ++ dbName  ++ ");"

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
                _ -> "\n, colWidths: [20, 50, 10, 20, 20]\n"
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
            middleend = "\t\t}//How do you like me now?\n"
            tablePush = "\t\ttable.push(generatePrettyRow(" ++ dbtablename ++ "));"
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
