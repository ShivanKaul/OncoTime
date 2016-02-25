{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinter where

import Types
import Lexer
import Data.List


class PrettyPrint a where
    prettyPrint :: a -> String

pretty :: (PrettyPrint a) => a -> String
pretty a = prettyPrint a

-- instance PrettyPrint (Expr t) where
--     prettyPrint (Neg _ a) = ("-" ++ (prettyPrint a)) 
--     prettyPrint (Var _ a) =  a
--     prettyPrint (IntConst _ a) = show a
--     prettyPrint (FloatConst _ a) = show a
--     prettyPrint (StringEx  _ a) = a
--     prettyPrint (Binary _ op a b) =  read $ (prettyPrint a) ++ (prettyPrint op) ++ (prettyPrint b)

-- instance PrettyPrint (Stmt u) where
--     prettyPrint (Seq a) = prettyPrint a
--     prettyPrint (If a b) =  "if " ++ (prettyPrint a) ++ " then " ++ (prettyPrint b) 
--     prettyPrint (IfElse a b c) = "if " ++ (prettyPrint a) ++ " then " ++ (prettyPrint b) ++ " else " ++ (prettyPrint c)
--     prettyPrint (While a b) =  "while " ++ (prettyPrint a) ++ " do " ++ (prettyPrint b) ++ " done" 
--     prettyPrint (Print a) = prettyPrint a ++ ";"
--     prettyPrint (Read a) =  prettyPrint a ++ ";"
--     prettyPrint (Assn a b) =  prettyPrint a ++ " = " ++  (prettyPrint b) ++ ";"
--     prettyPrint (IdStmt a) = a

-- instance PrettyPrint (Id a) where
--     prettyPrint (Val s  _) = s 



-- instance PrettyPrint BinOp where
--     prettyPrint Add = "+"
--     prettyPrint Multiply = "*"
--     prettyPrint Subtract = "-"
--     prettyPrint Divide = "/"

-- instance PrettyPrint Decl where
--     prettyPrint (DecSeq a) = prettyPrint a
--     prettyPrint (Dec a b) =  "var " ++ (a) ++ ": " ++ prettyPrint b ++ ";"



-- instance PrettyPrint Filter where
-- 	prettyPrint (Filter fname fdefs) = fname ++ " is " ++ map (prettyPrint fdefs)

-- instance PrettyPrint UseFileList where
-- 	prettyPrint (UseFileList u) = // change once we figure out what we want to do
-- with usefilelist

instance PrettyPrint (Docs) where
	prettyPrint (Docs docs) = "/* " ++ docs ++ " */"

instance PrettyPrint (Var) where
	prettyPrint (Var v) = v

instance PrettyPrint (ArgsList) where
    prettyPrint (varlist) = intercalate ", " (map prettyPrint varlist)

instance PrettyPrint Header where
    prettyPrint (Header fname argslist) = "script " ++ fname ++ "(" ++ prettyPrint argslist ++ ")"

-- instance PrettyPrint a => (PrettyPrint [a]) where
--     prettyPrint a = map prettyPrint a

-- instance PrettyPrint Program where
--     prettyPrint (Program header docs usefilelist  grouplist [filt] [comps]) 
--     	= (prettyPrint header) ++ (prettyPrint docs) ++ (prettyPrint usefilelist) ++ (prettyPrint [filt])
--     	 ++ (prettyPrint grouplist) ++ (prettyPrint [comps])