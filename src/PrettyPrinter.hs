{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import Types
import Lexer
import Data.List
import Parser


class PrettyPrint a where
    prettyPrint :: a -> String
    prettyIndent :: String -> a -> String

pretty :: (PrettyPrint a) => a -> String
pretty a = prettyPrint a

instance PrettyPrint PrintAction where
    prettyPrint (PrintVar var) = "print " ++ prettyPrint var
    prettyPrint (PrintTimeLine ptimeline) = "print timeline of " ++ prettyPrint ptimeline
    prettyPrint (PrintLength plength) = "print " ++ prettyPrint plength ++ ".length"
    prettyPrint (PrintFilters pfilters var) = "print " ++ (intercalate ", " pfilters) ++ 
        " of " ++ prettyPrint var
    prettyPrint (PrintElement v1 v2) = "print " ++ prettyPrint v1 ++ "[" ++ prettyPrint v2 ++ "]"


instance PrettyPrint TableAction where
    prettyPrint (TableCount var fname fval) = "table " ++ prettyPrint var ++ " = " ++
        "count " ++ fname ++ " by " ++ prettyPrint fval

instance PrettyPrint ForEachDef where
    prettyPrint (ForEachFilter fname var) = "foreach " ++ fname ++ " " ++ prettyPrint var 
    prettyPrint (ForEachTable var1 var2) = "foreach element " ++ prettyPrint var1 ++ " of " ++ prettyPrint var2 
    prettyPrint (ForEachSequence var seq) = "foreach sequence " ++ prettyPrint var ++ " like " ++ prettyPrint seq 
    prettyPrint (ForEachList var1 var2) = "foreach member " ++ prettyPrint var1 ++ 
        " in " ++ prettyPrint var2

instance PrettyPrint Event where
    prettyPrint (EAll ename) = ename
    prettyPrint (ESome ename vars) = ename ++ "(" ++ (intercalate ", " (map prettyPrint vars)) ++ ")"

instance PrettyPrint SeqField where
    prettyPrint (Single event) = prettyPrint event
    prettyPrint (Disj events) = "{" ++ (intercalate ", " (map prettyPrint events)) ++ "}"
    prettyPrint (Star events) = "{" ++ (intercalate ", " (map prettyPrint events)) ++ "}" ++ "*"
    prettyPrint (Neg event) = "(not" ++ prettyPrint event ++ ")"

instance PrettyPrint Sequence where
    prettyPrint seqs = "[" ++ (intercalate " | " (map prettyPrint seqs)) ++ "]"

instance PrettyPrint [SeqField] where
    prettyPrint seqs = (intercalate " -> " (map prettyPrint seqs))

instance PrettyPrint Computation where
    prettyIndent  (indent) (Foreach foreachdef comps) = indent ++ prettyPrint foreachdef ++ 
        " " ++ "\n" ++ indent ++ "{\n" ++ (intercalate "\n" (map (prettyIndent (indent ++ "\t")) comps)) ++ "\n" ++ indent ++ "}"
    prettyIndent (indent) (Table taction) = indent ++ prettyPrint taction
    prettyIndent (indent) (List var seq) = indent ++ "list " ++ prettyPrint var ++ " = " ++ "sequences like " 
        ++ prettyPrint seq
    prettyIndent (indent) (Print p) = indent ++ prettyPrint p
    prettyIndent (indent) (Barchart bchart) = indent ++ "barchart " ++ prettyPrint bchart

instance PrettyPrint GroupType where
    prettyPrint (GroupType g) = g

instance PrettyPrint GroupDefs where
    prettyPrint (Group gtype gvar gitems) = "group " ++ prettyPrint gtype ++ 
        " " ++ (prettyPrint gvar) ++ " " ++ " = " ++ "{ " ++
        (intercalate ", " (map prettyPrint gitems)) ++ " }"

instance PrettyPrint IntValue where
    prettyPrint i = show i

instance PrettyPrint RangeType where
    prettyPrint (Before i) = "Before " ++ prettyPrint i
    prettyPrint (After i) = "After " ++ prettyPrint i
    prettyPrint (Between i j) = prettyPrint i ++ " to " ++ prettyPrint j

instance PrettyPrint GroupItem where
    prettyPrint (GroupVal gval) = show gval
    prettyPrint (GroupVar gvar) = "<" ++ prettyPrint gvar ++ ">"
    prettyPrint (GroupRange grange) = prettyPrint grange

instance PrettyPrint FilterField where
    prettyPrint (FilterField ffield) = ffield

instance PrettyPrint FilterDef where
    prettyPrint (FilterDef ffield fvals) = "\t" ++ prettyPrint ffield ++ " : " ++ 
        (intercalate ", " (map prettyPrint fvals))

instance PrettyPrint Filter where
    prettyPrint (Filter fname fdefs) = fname ++ " is " ++ "\n" ++
        (intercalate "\n" (map prettyPrint fdefs))

instance PrettyPrint UseFile where
    prettyPrint (UseManyFile us) = "use " ++ (intercalate ", " (map (\x -> x ++ ".grp") us))

instance PrettyPrint (Docs) where
    prettyPrint (Docs docs) = "\n/*\n" ++ docs ++ "\n*/\n"

instance PrettyPrint (Var) where
    prettyPrint (Var v) = v

instance PrettyPrint Arg where
    prettyPrint (Arg groupType var) = prettyPrint groupType ++ " " ++ prettyPrint var

instance PrettyPrint Header where
    prettyPrint (Header fname argslist) = "script " ++ fname ++ "(" ++ 
        (intercalate ", " (map prettyPrint argslist)) ++ ")"

instance PrettyPrint [Computation] where
    prettyIndent indent comps = ((intercalate "\n" (map (prettyIndent indent) comps)) ++ "\n")

instance PrettyPrint [Filter] where
    prettyPrint filts = (intercalate "\n" (map prettyPrint filts)) ++ "\n"

instance PrettyPrint [UseFile] where
    prettyPrint ufiles = (intercalate "\n" (map prettyPrint ufiles)) ++ "\n"

instance PrettyPrint [GroupDefs] where
    prettyPrint groups = (intercalate "\n" (map prettyPrint groups)) ++ "\n"


instance PrettyPrint Program where
    prettyPrint (Program header docs usefilelist groups filt comps) 
     = (prettyPrint header) ++ (prettyPrint docs) ++ (prettyPrint usefilelist) ++ (prettyPrint groups)
     ++ (prettyPrint filt) ++ ("{\n" ++ (prettyIndent "\t" comps) ++ "}\n")

instance PrettyPrint TestProgram where
    -- prettyPrint (TestProgram2 header docs [usefiles] [groups] [filt] [comps]) = (prettyPrint header) ++ (prettyPrint docs) ++ (prettyPrint usefiles)
    prettyPrint (TestHeader header) = prettyPrint header
    prettyPrint (TestUseFileList usefilelist) = prettyPrint usefilelist
    prettyPrint (TestDocs docs) = prettyPrint docs
    prettyPrint (TestGroupList groups) = prettyPrint groups
    prettyPrint (TestComputation comps) = prettyPrint comps
    prettyPrint (TestFiltersList filters) = prettyPrint filters
    -- prettyPrint (TestHeader header) = prettyPrint header
