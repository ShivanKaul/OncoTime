{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Directory
import System.FilePath
import System.Exit
import System.Environment
import System.IO
import qualified Data.Map as Map
import Data.List
import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Pos
import TypeUtils
import Debug.Trace

--Our modules
import Types
import Parser
import PrettyPrinter
import Formatter
import Weeder

parseFile :: String -> IO ((Program Annotation),SourcePos)
parseFile filename =
    do
        program <- readFile filename
        checkForErrors program filename
        case parse ((oncoParser)<* eof) filename (formatFile program) of
            Left e ->
                do
                    die ("ERROR while parsing: " ++ show e) >> exitFailure
            Right parsedProg -> return parsedProg

checkForErrors :: String -> String -> IO()
checkForErrors prog file =
    do
        checkExtensionName file
        fileNameCheck file prog
        return ()

-- Check if file ends with .onc
checkExtensionName :: String -> IO()
checkExtensionName file =
    do
        if takeExtension file /= ".onc"
            then die ("ERROR: while reading " ++
                file ++ ": File extension not .onc") >> exitFailure
            else return ()

-- Check if script name == filename
fileNameCheck :: String -> String -> IO()
fileNameCheck file prog  =
    do
        -- Get script name
        case parse (getScriptName) "" prog of
            Left e -> die ("ERROR for file: " ++ (takeBaseName file) ++ show e)
            -- And die if != filename
            Right r -> if takeBaseName file /= r
                then do
                    die ("ERROR: while reading " ++
                        file ++
                        ": Filename does not match script name") >> exitFailure
                else return ()


prettyPrintFile :: (Program Annotation) -> String -> IO()
prettyPrintFile prog file =
    do
        writeFile (replaceExtension file ".pretty.onc") (pretty prog)
        putStrLn "VALID\n"

removeWildcardsFilters :: [Filter Annotation] -> [Filter Annotation]
removeWildcardsFilters filters =
    do
        map (removeWildcardsFieldDefs) filters

removeWildcardsFieldDefs :: Filter Annotation -> Filter Annotation
removeWildcardsFieldDefs (Filter fname fieldefs) =
    do
        Filter fname (foldl (\acc (FieldDef fielddefname fieldvals) -> case fieldvals of
            [GroupWildcard] -> acc
            _ -> (acc ++ [(FieldDef fielddefname fieldvals)]) ) [] fieldefs)


prettyPrintTypes :: (Program Annotation)-> String -> IO()
prettyPrintTypes (Program header docs usefilelist groups filt comps) file =
    do
        writeFile (replaceExtension file ".pptype.onc") ((prettyPrint header) ++
            (prettyPrint docs) ++
            (printHeadsPPTYPE header) ++ (prettyPrint groups) ++
            (printGroupsPPTYPE groups) ++
            (prettyPrint (removeWildcardsFilters filt)) ++
            (printFiltersPPTYPE filt) ++ ("{\n" ++ (prettyIndent "\t" comps) ++ "}\n"))
            -- ++ (printCompsPPTYPE comps))
        putStrLn ("Printed types for " ++ file ++ " in " ++ (replaceExtension file ".pptype.onc\n"))

parseString :: String -> (Program Annotation,SourcePos)
parseString str =
    case parse (oncoParser <* eof) "" str of
        Left e-> error $ show e
        Right r -> r

--pass in a list of strings and watch it become something amazing!!!
--ConfMap
tparseFile :: String ->IO ()
tparseFile file =
    do
        program <- readFile file
        case parse (testParser <* eof) file program of
            Left e ->
                do
                    hPutStrLn stderr ("ERROR: " ++ (show e)) >> exitFailure
            Right r -> print r

tparseString :: String -> (TestProgram Annotation)
tparseString str =
    case parse (testParser <* eof) "" str of
        Left e-> error$ show e
        Right r ->  r

tparseFileCheck :: String ->IO ()
tparseFileCheck file =
    do
        readData <- readFile "config.conf"
        let l= lines readData
        let totalMap =  Config $ configListToMap $ map makeConfig l
        program <- readFile file
        case parse ((testParserCheck totalMap) <* eof) file program of
            Left e ->
                do
                    hPutStrLn stderr "ERROR"
                    print e
                    >> exitFailure
            Right r -> print r
main =
    do
        -- oncotime filename flags
        (filename:flags) <- getArgs
        (parsed,pos) <- parseFile filename
        let symTabFun = if "-dumpsymtab" `elem` flags
            then
                (\a ->do {
                    let {symFile = replaceExtension filename ".symtab"};

                    putStrLn $ "Dumping symtable to " ++ symFile;
                    writeFile symFile a
                })
            else
                (\a->return ())


        weededProg <- weed filename symTabFun parsed pos
        prettyPrintFile parsed filename
        if "-pptype" `elem` flags then
            prettyPrintTypes weededProg filename
        else
            return ()

mainDebug filename = do
        (parsed,p) <- parseFile filename

        weededProg <- weed filename (pure $ pure ()) parsed p
        prettyPrintFile parsed filename
        prettyPrintTypes weededProg filename
