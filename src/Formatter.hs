module Formatter(formatFile) where

import Text.Regex.Posix
import Text.Regex
import Debug.Trace


occupiedLineRegex =  mkRegex "[^ \t\n][^\n]*\n"
docStringRegex =  "(/\\*(.|\n)*\\*/)" :: String
commentRegex =  mkRegex "//.*"
removeDocs :: String -> (String, String,String)
onlyemptyRegex = mkRegex "([ \t]*$)"
onlyemptyRegexN = mkRegex "([ \t]*\n)"
removeDocs fileContents =
  let matches = fileContents =~ docStringRegex :: MatchResult String

  in ((mrBefore matches),(mrMatch matches), (mrAfter matches))


handleLineComments line =
  let (stmnt,comm) = case matchRegexAll commentRegex (line) of
          Nothing ->  (line,"")
          Just(withoutComment, comm,_,_)-> ( (withoutComment,comm))
  in  if ((isEmpty stmnt) || null stmnt)
      then line
      else  let stmntWithoutNewline = if (  (last stmnt == '\n') )
                                     then init stmnt
                                     else stmnt
            in stmntWithoutNewline++";"++comm++"\n"


isEmpty :: String -> Bool
isEmpty line =  case ((matchRegexAll onlyemptyRegex line),(matchRegexAll onlyemptyRegexN line)) of
          (Nothing,Nothing) ->  False
          (Just ("",_,_,_),Just ("",_,_,_)) -> True
          _ -> False


removeNewLines :: String -> String
removeNewLines prog =
  case matchRegexAll occupiedLineRegex prog of
    Nothing -> handleLineComments prog
    Just(before, matched,after,_)-> (before)++(handleLineComments matched) ++(removeNewLines  after)


formatFile contents =
    let y@(h,d,p) = removeDocs contents
        h1 =  (removeNewLines h )
        p1 =  (removeNewLines p )
    in  {-  trace (addLineNumbers contents) -} (h1 ++ d ++ p1)

addLineNumbers contents=
  addnums 1 (lines contents) 
addnums :: Int->[String]  -> String
addnums num [] = []  
addnums num (l:ls) = ((show num) ++ "\t"++ l++"\n") ++ (addnums (num+1) ls)


testFormatter = (putStrLn $ addLineNumbers $ formatFile "File()\n\n/* ohai \ntutta*/\n\nhello! how are you? //kites\n//might\n\n\nbye\ntea\nkettle\n   \ncat\n\n\n\t\n") 



