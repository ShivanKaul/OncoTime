module Parser where

import System.FilePath
import System.Exit
import System.Environment
import System.IO
import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Token as Token


oncoParser:: Parser Program
oncoParser = 
    do
        whiteSpace
        hdr <- header
        docs <- 
        use <- 
        group <-
        filter <- 
        comp <-
        return $ Program hdr docs use group filter comp


--IO to checkfilename
header:: Parser Header
header = 
    do 
        reserved "script"
        fname <- filename
        args <- parens $ many var 
        return $ Header fname args

var 

