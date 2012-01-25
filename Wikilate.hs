-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia


module Wikilate where 


import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.List (intercalate, intersperse)
import Text.Regex (mkRegex, splitRegex)
import System.Environment (getArgs)
import System.Console.GetOpt
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP
import Network.URI


-- Main function

main = do
	argv <- getArgs
	(opts, args) <- parseCmdLineArgs argv

	when (length args < 1) $ error "No phrase specified."
	let phrase = head args

	translations <- fetchTranslations phrase opts
	putStrLn translations


-- Program options & command line

data Options = Options
	{ optSourceLang :: String
	, optDestLangs :: [String]
	}

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


-- Translating

newtype Translations = T [(String, String)]
instance Show Translations where
	show (T transAL) =
		intercalate "\n" $ translationsLines
		where translationsLines = map (\(lang, tr) -> lang ++ ": " ++ tr) transAL

fetchTranslations :: String -> Options -> IO String
fetchTranslations phrase opts = do
	let url = wikipediaUrl (optSourceLang opts) phrase
	let request = Request { rqURI = url, rqMethod = GET,
							rqHeaders = [], rqBody = "" }
	response <- simpleHTTP request
	case response of
		Left err -> error $ "Error connecting to Wikipedia: " ++ show err
		Right rsp ->
			case rspCode rsp of
				(2,_,_) -> return $ rspBody rsp
				otherwise -> error $ "Invalid HTTP response: " ++ show (rspCode rsp)


wikipediaUrl :: String -> String -> URI
wikipediaUrl sourceLang phrase =
	fromJust $ parseURI url
	where
		url = concat ["http://", sourceLang, ".wikipedia.org/w/api.php?", urlEncodeVars urlArgs]
		urlArgs = [("action", "query"), ("prop", "langlinks"), ("format", "json"), ("titles", phrase)]



-- Utilities

splitBy :: String -> String -> [String] -- yes, there is no `split` by default (!)
splitBy delim =
	splitRegex regexDelim
	where
		regexDelim = mkRegex $ escapeChars delim
		escapeChars = ('\\':) . intersperse '\\'


