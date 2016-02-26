module Formatter(testFormatter,formatFile) where
import Text.Regex.Posix


occupiedLineRegex =  "[^ \t\n][^\n]*\n" :: String
docStringRegex =  "(/\\*(.|\n)*\\*/)" :: String

removeDocs :: String -> (String, String,String)
removeDocs fileContents =
  let matches = fileContents =~ docStringRegex :: MatchResult String
      
  in ((mrBefore matches),(mrMatch matches), (mrAfter matches))

removeNewLines :: String -> String
removeNewLines prog =
  let matches = (prog =~ occupiedLineRegex :: [[String]])
  in foldr  (\s p -> let r = 
  					if (last s == '\n') 
  						then init s 
  						else s
  				in p++r++";\n") "" (concat matches)

formatFile contents =
	let (h,d,p) = removeDocs contents
	in 	let	h1 = removeNewLines h
			p1 = removeNewLines p
		in   (h1 ++ d++ p1)


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





--let caselessRegex = makeRegexOpts compilationOptions defaultExecOpt "[^ \t\n][^\n]*\n" 