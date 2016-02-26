module Formatter(formatFile) where
import Text.Regex.Posix
import Text.Regex


occupiedLineRegex =  mkRegex "[^ \t\n][^\n]*\n" 
docStringRegex =  "(/\\*(.|\n)*\\*/)" :: String
commentRegex =  mkRegex "//.*" :: String
removeDocs :: String -> (String, String,String)
removeDocs fileContents =
  let matches = fileContents =~ docStringRegex :: MatchResult String
      
  in ((mrBefore matches),(mrMatch matches), (mrAfter matches))

{-removeNewLines :: String -> String
removeNewLines prog =
    let matches = (prog =~ occupiedLineRegex :: [[String]])
    in foldr  (\s p -> 
        let match =  s =~ commentRegex :: MatchResult String
            s1 = mrBefore match
            comm = mrMatch match
            r = if (last s1 == '\n') 
                then init s1 
                else s1
        in r++";"++comm++"\n"++p) "" (concat matches)-}
handleLineComments line = 
  case matchRegexAll commentRegex line of
    Nothing -> prog
    Just(s1, comm,_,_)-> 
      let r = if (last s1 == '\n') 
              then init s1 
              else s1
      in r++";"++comm++"\n"
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
\hello!\n\
\\n\
\\n\
\\n\
\bye\n\
\\n\
\kettle\n"

