module Formatter(formatFile) where

import Text.Regex.Posix
import Text.Regex
import Debug.Trace


occupiedLineRegex =  mkRegex "[^ \t\n][^\n]*\n" 
docStringRegex =  "(/\\*(.|\n)*\\*/)" :: String
commentRegex =  mkRegex "//.*"
removeDocs :: String -> (String, String,String)
onlyemptyRegex = "([ \t]*(\n|$))"
removeDocs fileContents =
  let matches = fileContents =~ docStringRegex :: MatchResult String
      
  in ((mrBefore matches),(mrMatch matches), (mrAfter matches))


handleLineComments line = 
  let (stmnt,comm) = case matchRegexAll commentRegex (trace "" line) of
          Nothing ->  (line,"")
          Just(withoutComment, comm,_,_)-> ( (withoutComment,comm))
  in  if ((isEmpty stmnt) || null stmnt) 
      then line
      else  let stmntWithoutNewline = if (  (last stmnt == '\n') )
                                     then init stmnt 
                                     else stmnt
            in stmntWithoutNewline++";"++comm++"\n"

isEmpty line =(onlyemptyRegex=~line)::Bool


removeNewLines :: String -> String
removeNewLines prog =
  case matchRegexAll occupiedLineRegex prog of
    Nothing -> handleLineComments prog
    Just(before, matched,after,_)-> (before)++(handleLineComments matched) ++(removeNewLines  after) 


formatFile contents =
    let y@(h,d,p) = removeDocs contents
        h1 =  (removeNewLines h )
        p1 =  (removeNewLines p )
    in   (h1 ++ d ++ p1)


testFormatter = putStrLn $ show $ formatFile "File()\n/* ohai \ntutta*/\nhello!//kites\n//might\n\n\nbye\n\nkettle"

