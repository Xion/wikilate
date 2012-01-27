-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia


module Wikilate where 


import Control.Monad (when, mapM, liftM)
import Data.Maybe (fromJust)
import Data.List (intercalate, intersperse)
import Text.Regex (mkRegex, splitRegex)
import Text.JSON
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
    print translations


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
        (opts, params, [])  -> return (foldl (flip id) defaultOptions opts, params)
        (_, _, errorMsgs)   -> error $ concat ("\n":errorMsgs) ++ "\n" ++ usage
    where
        usage = usageInfo header cmdLineFlags
        header = "Usage: wikilate [OPTION...] phrase"


-- Data type to hold translations

newtype Translations = Translations [(String, String)]

instance Show Translations where
    show (Translations transAL) =
        intercalate "\n" $ translationsLines
        where translationsLines = map (\(lang, tr) -> lang ++ ": " ++ tr) transAL

(<+>) :: Translations -> Translations -> Translations
(Translations al1) <+> (Translations al2) = Translations $ al1 ++ al2


-- Retrieving translations

fetchTranslations :: String -> Options -> IO Translations
fetchTranslations phrase opts = do
    fetchTranslationsPart (optSourceLang opts) phrase Nothing
    where
        fetchTranslationsPart :: String -> String -> Maybe String -> IO Translations
        fetchTranslationsPart sourceLang phrase continue = do
            let url = wikipediaUrl sourceLang phrase continue
            let request = Request { rqURI = url, rqMethod = GET,
                                    rqHeaders = [], rqBody = "" }
            response <- simpleHTTP request
            case response of
                Left err -> error $ "Error connecting to Wikipedia: " ++ show err
                Right rsp ->
                    case rspCode rsp of
                        (2,_,_) -> do
                            let translations = parseTranslations $ rspBody rsp
                            case parseQueryContinue $ rspBody rsp of
                                Nothing -> return translations
                                Just qc -> do
                                    nextPart <- fetchTranslationsPart sourceLang phrase (Just qc)
                                    return $ translations <+> nextPart
                        otherwise -> error $ "Invalid HTTP response code: " ++ show (rspCode rsp)
                    
        parseQueryContinue :: String -> Maybe String
        parseQueryContinue jsonString =
            case readWikipediaJson jsonString of
                Ok c -> Just c
                Error _ -> Nothing
            where
                readWikipediaJson jsonString = let (!) = flip valFromObj in do
                    json <- decode jsonString
                    queryContinue <- json ! "query-continue"
                    langlinks <- queryContinue ! "langlinks"
                    let (JSString qc) = fromJust $ lookup "llcontinue" $ fromJSObject langlinks
                    return $ fromJSString qc


wikipediaUrl :: String -> String -> Maybe String -> URI
wikipediaUrl sourceLang phrase continue =
    fromJust $ parseURI url
    where
        url = concat ["http://", sourceLang, ".wikipedia.org/w/api.php?", urlEncodeVars urlArgs]
        urlArgs = [("action", "query"), ("prop", "langlinks"), ("format", "json"), ("titles", phrase)]
                  ++ maybe [] (\c -> [("llcontinue", c)]) continue


-- Parsing translations

parseTranslations :: String -> Translations
parseTranslations jsonString =
    case readWikipediaJson jsonString of
        Ok t -> t
        Error msg -> Translations $ []
    where
        readWikipediaJson jsonString = let (!) = flip valFromObj in do
            json <- decode jsonString
            query <- json ! "query"
            pages <- query ! "pages"
            let page = anySubobject pages
            let langlinks = fromJust $ lookup "langlinks" $ fromJSObject page
            readJSON langlinks
        anySubobject jsonObj = subObj
            where (_, (JSObject subObj)) = head $ fromJSObject jsonObj
            

instance JSON Translations where
    readJSON (JSArray jsonTrans) =
        liftM Translations $ mapM readJSONTranslation jsonTrans
        where
            readJSONTranslation (JSObject jt) = do
                langId <- jt ! "lang"
                translated <- jt ! "*"
                return (langId, translated)
            (!) = flip valFromObj
    showJSON = undefined


-- Utilities

splitBy :: String -> String -> [String] -- yes, there is no `split` by default (!)
splitBy delim =
    splitRegex regexDelim
    where
        regexDelim = mkRegex $ escapeChars delim
        escapeChars = ('\\':) . intersperse '\\'


