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


generateSQL :: (Program Annotation)->(DBConfig) ->(Config Annotation)-> String
generateSQL program@(Program header docs usefilelist groups filt comps) dbconf weedconf =
    do
        let diagnosis = (checkIfDiagnosis filt)
        let query = generateQueries filt dbconf diagnosis
        let displayFunction = generateDisplayFunction comps "patients" dbconf weedconf diagnosis
        generateScaffoldingJS query displayFunction

generatePrettyRowFunction :: String
generatePrettyRowFunction = "function generatePrettyRow(row) {\n\
            \\treturn Object.keys(row).map(function (key) {return row[key]});\n\
        \}\n\n"

generateDisplay :: [Computation Annotation] -> [String]
generateDisplay comps =
    do
        let compCodeList = map genCompCode comps
        compCodeList

generateDisplayFunction :: [Computation Annotation] ->String ->DBConfig->(Config Annotation)-> Maybe [String] -> String
generateDisplayFunction comps loopablename (DBConfig dbconfmap) (Config config) diag =
    do
        let
            (Just (FieldMap fieldmap)) =  M.lookup (loopablename, True) config
            fields = M.keys fieldmap
            tableHeaders = case diag of
                Nothing -> (delete "Diagnosis" (nub (map (\x -> dbconfmap M.! x) fields)))
                _ -> (nub (map (\x -> dbconfmap M.! x) fields))

            tableLeft = "\tvar table = new Table({\n\
                        \\t\thead: "
            tableRight = "\n\
                    \\t});\n"

            tableInit = tableLeft ++ (show tableHeaders) ++ tableRight

            forloopbegin = "\tfor (var i = 0; i < rows.length; i++) {\n"

            dbtablename = (dbconfmap M.! loopablename)
            middlestart = "\t\tvar "++ dbtablename ++" = {\n"
            c0 = foldl' (\prev currfield -> prev ++
                            "\t\t    "++
                            currfield++": rows[i]."++
                            -- GHETTO AF
                            (if currfield == "Diagnosis" then "Description" else currfield)
                            ++ ",\n") "" tableHeaders
            middleend = "\t\t}//How do you like me now?\n"
            tablePush = "\t\ttable.push(generatePrettyRow(" ++ dbtablename ++ "));\n\t}\n"
            forloopEnd ="\treturn table;\n"

        tableInit ++ forloopbegin++middlestart ++c0++middleend++tablePush++forloopEnd



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

genCompCode::Computation Annotation -> String
genCompCode _ = ""

generateQueries::[Filter Annotation]->DBConfig-> Maybe [String] -> [String]
generateQueries filterList (DBConfig dbconfmap) diag =
    do
        let columns = case diag of
                Nothing -> "*"
                _ -> "Patient.*, Diagnosis.Description"
        let queryString = "select " ++ columns ++ " from "
        --iterate through filter list
        queryList <- map (\(Filter filtName fdefList) ->
            do
                --add the filtername to the query
                let fromQuery = case diag of
                        Nothing -> (dbconfmap M.! filtName)
                        _ -> (dbconfmap M.! filtName) ++ ", Diagnosis"
                let joinClause = case diag of
                        Nothing -> ""
                        Just diagnoses -> generateWhereClauseForDiag diagnoses (DBConfig dbconfmap)
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
                            (generateWhereClauses (DBConfig dbconfmap) filterFieldsWithoutWildcard filtName)
                    -- regex to replace all AND AND by AND
                    let regexedWhere = subRegex (mkRegex " OR[ )]+AND ") whereQuery ") AND "
                    -- Hack for getting rid of last AND
                    selectQuery ++  (T.unpack (T.dropEnd 5 (T.pack regexedWhere)))
                ) filterList
        return queryList

generateWhereClauseForDiag :: [String] -> DBConfig -> String
generateWhereClauseForDiag diagnoses (DBConfig dbconfmap) =
    do
        let prefixDiags = "(Patient.PatientSerNum = Diagnosis.PatientSerNum) AND ("
        let whereDiags = map (\diagnosis -> "Diagnosis.DiagnosisCode like " ++
                "\"" ++ (dbconfmap M.! diagnosis) ++ "%\"") diagnoses
        prefixDiags ++ (intercalate " OR " whereDiags) ++ ") AND "


generateWhereClauses :: DBConfig->[FieldDef Annotation] -> String -> String
generateWhereClauses (DBConfig dbconfmap) fielddefs filtername =
    do

        foldl (\acc (FieldDef fname fvals) ->
            let
                tname = (dbconfmap M.! filtername) ++ "." ++ (dbconfmap M.! fname)
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
            \\t\t});\n\
            \\t}\n"

        let dbEnd = "\tdb.end();\n\
            \});\n\n"

        let dbDisplayFunctionStart = "function display(rows) {\n"

        let dbDisplayFunctionEnd = "}\n"

        let formatQueryList = map (\x -> dbQueryLeft ++ x ++ dbQueryRight ++
                dbDisplay ++ "\n") dbQueryList
        mysqlReq ++ tableReq ++ config ++ dbConnect ++ (concat formatQueryList) ++
            dbEnd ++ generatePrettyRowFunction ++ dbDisplayFunctionStart ++ dbDisplayFunction ++ dbDisplayFunctionEnd
