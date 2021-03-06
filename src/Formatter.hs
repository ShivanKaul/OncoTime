{-
OncoTime - Implementation of cancer-research domain-specific language as a project undertaken for
COMP 520 - Compiler Design in Winter 2016 at McGill University by

Shivan Kaul Sahib
Yusaira Khan
Brendan Games Gordon

The course was taught by Laurie Hendren.
 -}

module Formatter(formatFile, removeNewLines) where

import Text.Regex.Posix
import Text.Regex
import Debug.Trace


occupiedLineRegex =  mkRegex "[^ \t\n][^\n]*\n"
docStringRegex =  "(/\\*(.|\n)*\\*/)" :: String
commentRegex =  mkRegex "//.*"
removeDocs :: String -> (String, String,String)
onlyemptyRegex = mkRegex "([ \t\n]*$)"
onlyemptyRegexN = mkRegex "([ \t\n]*)"
removeDocs fileContents =
  let matches = fileContents =~ docStringRegex :: MatchResult String

  in ((mrBefore matches),(mrMatch matches), (mrAfter matches))


handleLineComments line =
  let (stmnt,comm) = case matchRegexAll commentRegex (line) of
          Nothing ->  (line,"")
          Just(withoutComment, comm,_,_)-> ( (withoutComment,comm))
  in  if ((isEmpty stmnt) || null stmnt)
      then line
      else  let stmntWithoutNewline = if (  (last (stmnt) == '\n') )
                                     then init stmnt
                                     else stmnt
            in stmntWithoutNewline++";"++comm++"\n"


isEmpty :: String -> Bool
isEmpty line =  case (matchRegexAll onlyemptyRegexN line) of
          Nothing ->  False
          Just ("",_,"",_) -> True
          _ ->  False


removeNewLines :: String -> String
removeNewLines prog =
  case matchRegexAll occupiedLineRegex prog of
    Nothing ->  handleLineComments prog
    Just(before, matched,after,_)-> (before)++(handleLineComments matched) ++( removeNewLines  after)

formatFile :: String -> String
formatFile contents =
    let y@(h,d,p) = removeDocs contents
        h1 =  (removeNewLines h )
        p1 =  (removeNewLines p )
        all = (h1 ++ d ++ p1)
    in {- trace (addLineNumbers all)-} all

addLineNumbers contents=
  addnums 1 (lines contents)
addnums :: Int->[String]  -> String
addnums num [] = []
addnums num (l:ls) = ((show num) ++ "\t"++ l++"\n") ++ (addnums (num+1) ls)


testFormatter = (putStrLn $ addLineNumbers $ formatFile "File()\n\n/* ohai \ntutta*/\n\nhello! how are you? //kites\n//might\n\n\nbye\ntea\nkettle\n   \n\n\ncat")
