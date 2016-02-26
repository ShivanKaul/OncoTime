module Formatter(formatFile) where

import Text.Regex.Posix
import Text.Regex
import Debug.Trace


occupiedLineRegex =  mkRegex "[^ \t\n][^\n]*\n" 
docStringRegex =  "(/\\*(.|\n)*\\*/)" :: String
commentRegex =  mkRegex "//.*"
removeDocs :: String -> (String, String,String)
removeDocs fileContents =
  let matches = fileContents =~ docStringRegex :: MatchResult String
      
  in ((mrBefore matches),(mrMatch matches), (mrAfter matches))


handleLineComments line = 
  let (stmnt,comm) = case matchRegexAll commentRegex line of
          Nothing -> (line,"")
          Just(withoutComment, comm,_,_)-> (withoutComment,comm)
      stmntWithoutNewline = if ((stmnt/="")&&(last stmnt == '\n') )
              then init stmnt 
              else stmnt
      in stmntWithoutNewline++";"++comm++"\n"

removeNewLines :: String -> String
removeNewLines prog =
  case matchRegexAll occupiedLineRegex prog of
    Nothing -> prog
    Just(before, matched,after,_)-> before++(handleLineComments matched) ++(removeNewLines  after) 


formatFile contents =
    let y@(h,d,p) = removeDocs contents
        h1 = removeNewLines h 
        p1 = removeNewLines p 
    in   (h1 ++ d++ "\n" ++ p1)


testFormatter = putStrLn $ formatFile "\
\File()\n\
\/* ohai \n\
\tutta*/\n\
\hello!//kites\n\
\\n\
\\n\
\\n\
\bye\n\
\\n\
\kettle\n"

