{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import Types
import Lexer
import Data.List
import Parser


class PrettyPrint a where
    prettyPrint :: a -> String
    prettyPrint _ = ""
    prettyIndent :: String -> a -> String
    prettyIndent _ _ = ""

pretty :: (PrettyPrint a) => a -> String
pretty program = prettyPrint program

printTypesGroups :: [(GroupDefs Annotation)] -> String
printTypesGroups groups =
    do
        let groupListString = map (\(Group (GroupType t) (Var v _ ) _) ->
                                ("// Group " ++ v ++ " : " ++ t)) groups
        (intercalate "\n" groupListString)

-- printTypesFilters :: [Filter] -> String
-- printTypesFilters filters =
--     do
--         let filtersListString = map (\(Filter filtername (Var v) _) ->
--                                 (v ++ " : " ++ t)) filtersListString
--         (intercalate "\n" filtersListString)

-- printTypesVars :: [Var] -> String
-- printTypesVars vars =
    -- do


printTypes :: (Program Annotation) -> String
printTypes (Program _ _ _ groups filt comps) =
    do
        "// PRINTING GROUPS\n" ++ (printTypesGroups groups)-- ++ (printTypesFilters filt)

instance PrettyPrint (PrintAction Annotation) where
    prettyPrint (PrintVar var) = "print " ++ prettyPrint var
    prettyPrint (PrintTimeLine ptimeline) = "print timeline of " ++ prettyPrint ptimeline
    prettyPrint (PrintLength plength) = "print " ++ prettyPrint plength ++ ".length"
    prettyPrint (PrintFilters pfilters var) = "print " ++ (intercalate ", " pfilters) ++
        " of " ++ prettyPrint var
    prettyPrint (PrintElement v1 v2) = "print " ++ prettyPrint v1 ++ "[" ++ prettyPrint v2 ++ "]"


-- instance PrettyPrint TableAction where
--     prettyPrint (TableCount var fname fval) = "table " ++ prettyPrint var ++ " = " ++
--         "count " ++ fname ++ " by " ++ prettyPrint fval

instance PrettyPrint (ForEachDef Annotation) where
    prettyPrint (ForEachFilter fname var) = "foreach " ++ fname ++ " " ++ prettyPrint var 
    prettyPrint (ForEachTable var1 var2) = "foreach element " ++ prettyPrint var1 ++ " of " ++ prettyPrint var2 
    prettyPrint (ForEachSequence var seq) = "foreach sequence " ++ prettyPrint var ++ " like " ++ prettyPrint seq 
    prettyPrint (ForEachList var1 var2) = "foreach member " ++ prettyPrint var1 ++ 
        " in " ++ prettyPrint var2

instance PrettyPrint (Event Annotation) where
    prettyPrint (Event ename a) = ename

instance PrettyPrint (SeqField Annotation) where
    prettyPrint (Single event) = prettyPrint event
    prettyPrint (Comma events) = "{" ++ (intercalate ", " (map prettyPrint events)) ++ "}"
    prettyPrint (Bar events) = (intercalate " | " (map prettyPrint events))
    prettyPrint (Star events) = "{" ++ (intercalate ", " (map prettyPrint events)) ++ "}" ++ "*"
    prettyPrint (Neg event) = "(not" ++ prettyPrint event ++ ")"

instance PrettyPrint ([(SeqField Annotation)] ) where
    prettyPrint seqs = "[ " ++ (intercalate " -> " (map prettyPrint seqs)) ++ " ]"



instance PrettyPrint (Computation Annotation) where
    prettyIndent  (indent) (Foreach foreachdef comps) = indent ++ prettyPrint foreachdef ++ " " ++ "\n" ++ indent ++ "{\n" ++ (intercalate "\n" (map (prettyIndent (indent ++ "\t")) comps)) ++ "\n" ++ indent ++ "}"
    prettyIndent (indent) (Table var fname ffield) = indent ++"table " ++ prettyPrint var ++ " = " ++
        "count " ++ fname ++ " by " ++ prettyPrint ffield
    prettyIndent (indent) (List var seq) = indent ++ "list " ++ prettyPrint var ++ " = " ++ "sequences like "
    prettyIndent (indent) (Print p) = indent ++ prettyPrint p
    prettyIndent (indent) (Barchart bchart) = indent ++ "barchart " ++ prettyPrint bchart

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
