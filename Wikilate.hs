-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia


module Wikilate where 


import Data.List (intersperse)
import Text.Regex (mkRegex, splitRegex)
import System.Environment (getArgs)
import System.Console.GetOpt


-- Utilities

splitBy :: String -> String -> [String] -- yes, there is no `split` by default (!)
splitBy delim =
	splitRegex regexDelim
	where
		regexDelim = mkRegex $ escapeChars delim
		escapeChars = ('\\':) . intersperse '\\'


-- Command line arguments

data CmdLineFlag = SourceLang String | DestLang [String]
	deriving Show

cmdLineFlags :: [OptDescr CmdLineFlag]
cmdLineFlags =
	[ Option ['s'] ["source"] (ReqArg SourceLang "LANG") "language of the source phrase"
	, Option ['d'] ["dest"] (ReqArg (DestLang . splitBy ",") "LANG[,LANG[,...]]") "destination language(s)"
	]


parseCmdLineArgs :: [String] -> IO ([CmdLineFlag], [String])
parseCmdLineArgs args = 
	case getOpt Permute cmdLineFlags args of
		(opts, params, [])	-> return (opts, params)
		(_, _, errorMsgs)	-> ioError (userError (concat errorMsgs ++ usage))
	where
		usage = usageInfo header cmdLineFlags
		header = "Usage: wikilate [OPTION...] phrase"


-- Main function

main = do
	argv <- getArgs
	(flags, args) <- parseCmdLineArgs argv
	print (flags, args)