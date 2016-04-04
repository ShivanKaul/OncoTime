{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import Types
import Lexer
import Data.List
import Parser
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Parsec.String
import Text.Regex
import Debug.Trace

class PrettyPrint a where
    prettyPrint :: a -> String
    prettyPrint _ = ""
    prettyIndent :: String -> a -> String
    prettyIndent _ _ = ""
    prettyAnnotated ::  a -> String
    prettyAnnotated  _ = ""

pretty :: (PrettyPrint a) => a -> String
pretty program = prettyPrint program

generateSQL :: (Program Annotation)->(DBConfig) ->(Config Annotation)-> String
generateSQL program@(Program header docs usefilelist groups filt comps) dbconf weedconf =
    do
        -- Diagnosis?
        -- If yes, do a join and select
        -- show filt
        let diagnosis = (checkIfDiagnosis filt)
        let query = generateQueries filt dbconf diagnosis
        let displayFunction = generateDisplayFunction comps "patients" dbconf weedconf diagnosis
        generateScaffoldingJS query displayFunction


-- TODO: make this more meaningful using comps
generateDisplay :: [Computation Annotation] -> [String]
generateDisplay comps =
    do

        let compCodeList = map genCompCode comps

        return compCodeList


{-
        let forloop = "\tfor (var i = 0; i < rows.length; i++) {\n\
                \\t\tvar Patient = {\n\
                \\t\t    id: rows[i].PatientSerNum,\n\
                \\t\t    dob: rows[i].DateOfBirth,\n\
=======
generateDisplayFunction :: [Computation Annotation] ->String ->DBConfig->(Config Annotation)-> String
generateDisplayFunction comps loopablename (DBConfig dbconfmap) (Config config) =
=======
checkIfDiagnosis :: [Filter Annotation] -> Maybe [String]
checkIfDiagnosis filters =
    do
        foldl (\acc cur@(Filter _ fdefs) -> foldl (\accInner (FieldDef fname fvals) ->
            if fname == "diagnosis" then
                Just (map (\(GroupValString name _) -> name) fvals)
            else
                accInner) acc fdefs)
                 Nothing filters


generateDisplayFunction :: [Computation Annotation] ->String ->DBConfig->(Config Annotation)-> Maybe [String] -> String
generateDisplayFunction comps loopablename (DBConfig dbconfmap) (Config config) diag =
>>>>>>> f23842e... Diagnosis joins
    do
        let
            forloopbegin = "\tfor (var i = 0; i < rows.length; i++) {\n"
            (Just (FieldMap fieldmap)) =  M.lookup (loopablename, True) config
            fields = M.keys fieldmap
            dbtablename = (dbconfmap M.! loopablename)
            middlestart = "\t\tvar "++ dbtablename ++" = {\n"
            -- middlestart = "\t\tvar "++ dbtablename ++" = rows[i]\n"
            c0 = foldl' (\prev currfield -> prev ++
<<<<<<< HEAD
                            if currfield == "diagnosis"
                            then ""
                            else "\t\t    "++currfield++": rows[i]."++(dbconfmap M.! currfield)++",\n") "" fields
            c1 ="\t\t    id: rows[i].PatientSerNum,\n\
            \\t\t    dob: rows[i].DateOfBirth,\n\
>>>>>>> 5376f16... Rename display function
                \\t\t    sex: rows[i].Sex,\n\
                \\t\t    postalcode: rows[i].PostalCode\n"
=======
                            "\t\t    "++
                            currfield++": rows[i]."++
                            -- GHETTO AF
                            (if currfield == "diagnosis" then "Description" else (dbconfmap M.! currfield))
                            ++ ",\n") "" fields
>>>>>>> f23842e... Diagnosis joins
            middleend = "\t\t}//How do you like me now?\n"
            forloopEnd ="\t\tprocess.stdout.write('"++dbtablename++" : ');\n\
                \\t\tconsole.log("++dbtablename++");\n\
            \\t}\n"
<<<<<<< HEAD
        forloopbegin++middlestart++c0++middleend++forloopEnd
-}

genCompCode::Comptutation Annotation -> String

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
                (GroupDate dd mm yy) -> acc ++ fname ++" " ++  dd++"-"++mm++"-"++yy++ " OR "
                ) "" fvals
        expanded


generateScaffoldingJS :: [String] -> String -> String
generateScaffoldingJS dbQueryList dbDisplayFunction =
    do
        let mysqlReq = "var mysql = require('mysql');\n"
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
        let dbDisplay = "\t\t\tconsole.log(display(rows));\n\
            \\t\t});\n\
            \\t}\n"

        let dbEnd = "\tdb.end();\n\
            \});\n\n"

        let dbDisplayFunctionStart = "function display(rows) {\n"

        let dbDisplayFunctionEnd = "\n}\n"

        let formatQueryList = map (\x -> dbQueryLeft ++ x ++ dbQueryRight ++ dbDisplay ++ "\n") dbQueryList
        mysqlReq ++ config ++ dbConnect ++ (concat formatQueryList) ++dbEnd ++ dbDisplayFunctionStart ++ dbDisplayFunction ++ dbDisplayFunctionEnd

printTypesGroups :: [(GroupDefs Annotation)] -> String
printTypesGroups groups =
    do
        let groupListString = map (\(Group (GroupType t) (Var v _ ) _) ->
                                ("// Group " ++ v ++ " : " ++ t ++ "\n")) groups
        (intercalate "\n" groupListString)

printTypesHeader :: (Header Annotation) -> String
printTypesHeader (Header name args) =
    do
        let argsListString = map (\(Arg (GroupType t) (Var v _ )) ->
                                ("// Header param " ++ v ++ " : " ++ t ++ "\n")) args
        (intercalate "\n" argsListString)

printTypesFilters :: [(Filter Annotation)] -> String
printTypesFilters filters =
    do
        intercalate "\n//" (map (T.unpack) (T.split (=='\n') (T.pack ("//" ++ prettyPrint filters)))) ++ "\n"

printGroupsPPTYPE :: ([GroupDefs Annotation]) -> String
printGroupsPPTYPE groups =
    do
        "// PRINTING GROUPS\n" ++ (printTypesGroups groups)

printHeadsPPTYPE :: (Header Annotation) -> String
printHeadsPPTYPE header =
    do
        "// PRINTING HEADER PARAMS\n" ++ (printTypesHeader header)
printFiltersPPTYPE :: ([Filter Annotation]) -> String
printFiltersPPTYPE filters =
    do
        "// PRINTING FILTERS\n" ++ (printTypesFilters filters)

instance PrettyPrint (PrintAction Annotation) where
    prettyPrint (PrintVar var) = "print " ++ prettyPrint var ++" // "++prettyAnnotated var
    prettyPrint (PrintTimeLine ptimeline) = "print timeline of " ++ prettyPrint ptimeline++ " // "++prettyAnnotated ptimeline
    prettyPrint (PrintLength plength) = "print " ++ prettyPrint plength ++ ".length" ++ " // "++prettyAnnotated plength
    prettyPrint (PrintFilters pfilters var) = "print " ++ (intercalate ", " pfilters) ++    " of " ++ prettyPrint var ++  " // "++prettyAnnotated var
    prettyPrint (PrintElement v1 v2) = "print " ++ prettyPrint v1 ++ "[" ++ prettyPrint v2 ++ "]" ++ " // "++prettyAnnotated v1++  " "++prettyAnnotated v2



-- instance PrettyPrint TableAction where
--     prettyPrint (TableCount var fname fval) = "table " ++ prettyPrint var ++ " = " ++
--         "count " ++ fname ++ " by " ++ prettyPrint fval

instance PrettyPrint (ForEachDef Annotation) where
    prettyPrint (ForEachFilter fname var) = "foreach " ++ fname ++ " " ++ prettyPrint var ++ " // "++prettyAnnotated var
    prettyPrint (ForEachTable var1 var2) = "foreach element " ++ prettyPrint var1 ++ " of " ++ prettyPrint var2  ++ " // "++prettyAnnotated var1 ++" "++prettyAnnotated var
    prettyPrint (ForEachSequence var sequ) = "foreach sequence " ++ prettyPrint var ++ " like " ++ prettyPrint sequ ++ " // "++prettyAnnotated var ++ " "++prettyAnnotated sequ
    prettyPrint (ForEachList var1 var2) = "foreach member " ++ prettyPrint var1 ++
        " in " ++ prettyPrint var2 ++ " // "++prettyAnnotated var1 ++ " "++prettyAnnotated var2

instance PrettyPrint (Event Annotation) where
    prettyPrint (Event ename a) = ename

instance PrettyPrint (SeqField Annotation) where
    prettyPrint (Single event) = prettyPrint event
    prettyPrint (Comma events) = "{" ++ (intercalate ", " (map prettyPrint events)) ++ "}"
    prettyPrint (Bar events) = (intercalate " | " (map prettyPrint events))
    prettyPrint (Star events) = "{" ++ (intercalate ", " (map prettyPrint events)) ++ "}" ++ "*"
    prettyPrint (Neg event) = "(not " ++ prettyPrint event ++ ")"

instance PrettyPrint ([(SeqField Annotation)] ) where
    prettyPrint seqs = "[ " ++ (intercalate " -> " (map prettyPrint seqs)) ++ " ]"



instance PrettyPrint (Computation Annotation) where
    prettyIndent  (indent) (Foreach foreachdef comps _) = indent ++ prettyPrint foreachdef ++ " " ++ "\n" ++ indent ++ "{\n" ++ (intercalate "\n" (map (prettyIndent (indent ++ "\t")) comps)) ++ "\n" ++ indent ++ "}"
    prettyIndent (indent) (Table var fname ffield) = indent ++"table " ++ prettyPrint var ++ " = " ++
        "count " ++ fname ++ " by " ++ prettyPrint ffield
    prettyIndent (indent) (List var seq) = indent ++ "list " ++ prettyPrint var ++ " = " ++ "sequences like "
    prettyIndent (indent) (Print p) = indent ++ prettyPrint p
    prettyIndent (indent) (Barchart bchart) = indent ++ "barchart " ++ prettyPrint bchart ++" // "++prettyAnnotated barchart


instance PrettyPrint GroupType where
    prettyPrint (GroupType g) = g

instance PrettyPrint (GroupDefs Annotation) where
    prettyPrint (Group gtype gvar gitems) = "group " ++ prettyPrint gtype ++ " " ++ (prettyPrint gvar) ++ " " ++ " = " ++ "{ " ++
        (intercalate ", " (map prettyPrint gitems)) ++ " }"

instance PrettyPrint IntValue where
    prettyPrint i = show i

instance PrettyPrint (RangeType Annotation) where
    prettyPrint (Before i a) = "Before " ++ prettyPrint i
    prettyPrint (After i a) = "After " ++ prettyPrint i
    prettyPrint (Between i j a) = prettyPrint i ++ " to " ++ prettyPrint j
    prettyPrint (SingleInt g a) = prettyPrint g

instance PrettyPrint (GroupItem Annotation) where
    prettyPrint (GroupValString gval a) = gval
    prettyPrint (GroupDate y m d a) = (prettyPrint y) ++"-"++ (prettyPrint m) ++"-"++ (prettyPrint d)
    prettyPrint (GroupVar gvar) = "<" ++ prettyPrint gvar ++ ">"
    prettyPrint (GroupRange grange) = prettyPrint grange
    prettyPrint (GroupWildcard) = "*"

instance PrettyPrint FieldName where
    prettyPrint (ffield) = ffield

instance PrettyPrint (FieldDef Annotation) where
    prettyPrint (FieldDef ffield fvals) = "\t" ++ prettyPrint ffield ++ " : " ++ (intercalate ", " (map prettyPrint fvals))

instance PrettyPrint (Filter Annotation) where
    prettyPrint (Filter fname fdefs) = fname ++ " is" ++ "\n" ++
        (intercalate "\n" (map prettyPrint fdefs))

instance PrettyPrint UseFile where
    prettyPrint (UseFile us) = "use " ++ (intercalate ", " (map (\x -> x ++ ".grp") us))

instance PrettyPrint (Docs) where
    prettyPrint (Docs docs) = "\n/**" ++ docs ++ "*/\n"

instance PrettyPrint (Var Annotation) where
    prettyPrint (Var v a) = v
    prettyAnnotated  (Var v (Annotation a))= "TYPE ("++v++" : "++a ++")"

instance PrettyPrint  ( Parser(Var Annotation)) where
    prettyAnnotated  _ = ""

instance PrettyPrint (Arg Annotation) where
    prettyPrint (Arg groupType var) = prettyPrint groupType ++ " " ++ prettyPrint var

instance PrettyPrint (Header Annotation) where
    prettyPrint (Header fname argslist) = "script " ++ fname ++ "(" ++ (intercalate ", " (map prettyPrint argslist)) ++ ")"

instance PrettyPrint [(Computation Annotation)] where
    prettyIndent indent comps = ((intercalate "\n" (map (prettyIndent indent) comps)) ++ "\n")

instance PrettyPrint [(Filter Annotation)] where
    prettyPrint filts = (intercalate "\n" (map prettyPrint filts)) ++ "\n"

instance PrettyPrint [(UseFile)] where
    prettyPrint ufiles = (intercalate "\n" (map prettyPrint ufiles)) ++ "\n"

instance PrettyPrint [(GroupDefs Annotation)] where
    prettyPrint groups = (intercalate "\n" (map prettyPrint groups)) ++ "\n"


instance PrettyPrint (Program Annotation) where
    prettyPrint (Program header docs usefilelist groups filt comps) = (prettyPrint header) ++ (prettyPrint docs) ++ (prettyPrint usefilelist) ++ (prettyPrint groups)
     ++ (prettyPrint filt) ++ ("{\n" ++ (prettyIndent "\t" comps) ++ "}\n")

instance PrettyPrint (TestProgram Annotation) where
    -- prettyPrint (TestProgram2 header docs [usefiles] [groups] [filt] [comps]) = (prettyPrint header) ++ (prettyPrint docs) ++ (prettyPrint usefiles)
    prettyPrint (TestHeader header) = prettyPrint header
    prettyPrint (TestUseFileList usefilelist) = prettyPrint usefilelist
    prettyPrint (TestDocs docs) = prettyPrint docs
    prettyPrint (TestGroupList groups) = prettyPrint groups
    prettyPrint (TestComputation comps) = prettyPrint comps
    prettyPrint (TestFiltersList filters) = prettyPrint filters
    -- prettyPrint (TestHeader header) = prettyPrint header
instance PrettyPrint (ComputationType) where
    -- prettyPrint (TestProgram2 header docs [usefiles] [groups] [filt] [comps]) = (prettyPrint header) ++ (prettyPrint docs) ++ (prettyPrint usefiles)
    prettyPrint (TFilter s) = "Single "++ s
    prettyPrint (t) = tail $ show t
