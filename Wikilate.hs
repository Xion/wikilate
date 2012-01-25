-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia


module Wikilate where 


import Data.List (intersperse)
import Text.Regex (mkRegex, splitRegex)
import System.Environment (getArgs)
import System.Console.GetOpt
import Network.HTTP.Base (urlEncodeVars)


-- Main function

main = do
	argv <- getArgs
	(flags, args) <- parseCmdLineArgs argv
	print (flags, args)


-- Program options & command line

data Options = Options
	{ optSourceLang :: String
	, optDestLangs :: [String]
	}
	deriving (Show)	-- for debugging

defaultOptions = Options
	{ optSourceLang = "en"
	, optDestLangs = ["de", "fr", "es", "pl"]
	}

cmdLineFlags :: [OptDescr (Options -> Options)]
cmdLineFlags =
	[ Option ['s'] ["source"]
	  (ReqArg (\f opts -> opts { optSourceLang = f }) "LANG")
	  "language of the source phrase"
	, Option ['d'] ["dest"]
	  (ReqArg (\f opts -> opts { optDestLangs = splitBy "," f }) "LANG[,LANG[,...]]")
	  "destination language(s)"
	]

parseCmdLineArgs :: [String] -> IO (Options, [String])
parseCmdLineArgs args = 
	case getOpt Permute cmdLineFlags args of
		(opts, params, [])	-> return (foldl (flip id) defaultOptions opts, params)
		(_, _, errorMsgs)	-> error $ concat ("\n":errorMsgs) ++ "\n" ++ usage
	where
		usage = usageInfo header cmdLineFlags
		header = "Usage: wikilate [OPTION...] phrase"


-- Actual logic

wikipediaUrl :: String -> String -> String
wikipediaUrl sourceLang phrase =
	concat ["http://", sourceLang, ".wikipedia.org/w/api.php?", urlEncodeVars urlArgs]
	where
		urlArgs = [("action", "query"), ("prop", "langlinks"), ("titles", phrase)]




-- Utilities

splitBy :: String -> String -> [String] -- yes, there is no `split` by default (!)
splitBy delim =
	splitRegex regexDelim
	where
		regexDelim = mkRegex $ escapeChars delim
		escapeChars = ('\\':) . intersperse '\\'


