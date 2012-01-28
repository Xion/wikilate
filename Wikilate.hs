-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia

{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Main where 


import Prelude hiding (catch)

import Control.Monad (when, mapM, liftM)
import Control.Exception (catch, IOException)
import Data.Maybe (fromJust)
import Data.List (intercalate, intersperse)
import Data.Monoid
import Text.Regex (mkRegex, splitRegex)
import Text.JSON
import System.Environment (getArgs)
import System.Console.GetOpt
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP
import Network.URI


-- Main function

main :: IO ()
main = do
    argv <- getArgs
    (opts, args) <- parseCmdLineArgs argv

    when (length args < 1) $ error "No phrase specified."
    let phrase = intercalate " " args

    catch (fetchTranslations phrase opts >>= print) $ \e ->
        putStrLn $ "<Could not obtain translations: " ++ (show (e :: IOException)) ++ ">"


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
                       deriving (Monoid)

(<&>) :: Translations -> [String] -> Translations
(Translations al) <&> list = Translations $ filter ((`elem` list) . fst) al

instance Show Translations where
    show (Translations transAL) =
        intercalate "\n" $ translationsLines
        where translationsLines = map (\(lang, tr) -> lang ++ ": " ++ tr) transAL


-- Retrieving translations

fetchTranslations :: String -> Options -> IO Translations
fetchTranslations phrase opts = do
    translations <- fetchTranslationsPart (optSourceLang opts) phrase Nothing
    return $ translations <&> optDestLangs opts
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
                            translations <- parseTranslations $ rspBody rsp
                            case parseQueryContinue $ rspBody rsp of
                                Nothing -> return translations
                                Just qc -> do
                                    nextPart <- fetchTranslationsPart sourceLang phrase (Just qc)
                                    return $ translations `mappend` nextPart
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
                    maybe (fail "Invalid query-continue specifier") (\ll ->
                        case ll of
                            JSString qc -> return $ fromJSString qc
                            otherwise -> fail "No query-continue specifier found"
                        ) $ lookup "llcontinue" $ fromJSObject langlinks


wikipediaUrl :: String -> String -> Maybe String -> URI
wikipediaUrl sourceLang phrase continue =
    fromJust $ parseURI url
    where
        url = concat ["http://", sourceLang, ".wikipedia.org/w/api.php?", urlEncodeVars urlArgs]
        urlArgs = [("action", "query"), ("prop", "langlinks"), ("format", "json"), ("titles", phrase)]
                  ++ maybe [] (\c -> [("llcontinue", c)]) continue


-- Parsing translations

parseTranslations :: String -> IO Translations
parseTranslations jsonString =
    case readWikipediaJson jsonString of
        Ok t -> return t
        Error msg -> fail msg
    where
        readWikipediaJson jsonString = let (!) = flip valFromObj in do
            json <- decode jsonString
            query <- json ! "query"
            pages <- query ! "pages"
            let page = anySubobject pages
            let langlinks = lookup "langlinks" $ fromJSObject page
            maybe (fail "No langlinks found in JSON") readJSON $ langlinks
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
