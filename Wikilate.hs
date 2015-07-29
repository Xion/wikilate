-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import Prelude hiding (catch)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, mapM, void)
import Control.Exception (catch, IOException)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Monoid
import Text.JSON
import System.Environment (getArgs)
import System.Console.GetOpt
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP
import Network.URI


{-# ANN module "HLint: ignore Use string literal" #-}


-- Main function

main = do
    argv <- getArgs
    (opts, args) <- parseCmdLineArgs argv

    when (length args < 1) $
        error "No phrase specified."
    let phrase = unwords args

    catch (void $ fetchTranslations phrase opts) handleError
  where
    handleError :: IOException -> IO ()
    handleError e =
        putStrLn $ "<Could not obtain translations: " ++ show e ++ ">"


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
      (ReqArg (\f opts -> opts { optDestLangs = splitOn "," f })
              "LANG[,LANG[,...]]")
      "destination language(s)"
    ]

parseCmdLineArgs :: [String] -> IO (Options, [String])
parseCmdLineArgs args =
    case getOpt Permute cmdLineFlags args of
        (opts, params, []) ->
            return (foldl (flip id) defaultOptions opts, params)
        (_, _, errorMsgs) ->
            error $ concat ("\n":errorMsgs) ++ "\n" ++ usage
  where
    usage = usageInfo header cmdLineFlags
    header = "Usage: wikilate [OPTION...] phrase"


-- | Holds translations as a mapping of (language, text),
newtype Translations = Translations [(String, String)]
                       deriving (Monoid, Eq)

-- | Filter translations through a list of languages.
(<&>) :: Translations -> [String] -> Translations
(Translations al) <&> list =
    Translations $ filter ((`elem` list) . fst) al

instance Show Translations where
    show (Translations transAL) =
        unlines translationsLines
      where
        translationsLines = map (\(lang, tr) -> lang ++ ": " ++ tr) transAL


-- | Retrieves translations of given phrase.
fetchTranslations :: String -> Options -> IO Translations
fetchTranslations phrase Options{..} =
    fetchTranslationsPart phrase Nothing
  where
    fetchTranslationsPart :: String -> Maybe String -> IO Translations
    fetchTranslationsPart phrase continue = do
        let url = wikipediaUrl optSourceLang phrase continue
        let request = Request { rqURI = url
                              , rqMethod = GET
                              , rqHeaders = []
                              , rqBody = ""
                              }
        response <- simpleHTTP request
        case response of
            Left err -> error $ "Error connecting to Wikipedia: " ++ show err
            Right rsp -> handleWikipediaResponse rsp

    handleWikipediaResponse rsp =
        case rspCode rsp of
            (2,_,_) -> do
                translations <- parseTranslations $ rspBody rsp
                let filtered = translations <&> optDestLangs
                when (filtered /= mempty) $
                    print filtered
                case parseQueryContinue $ rspBody rsp of
                    Error _ -> return translations
                    Ok qc -> do
                        nextPart <- fetchTranslationsPart phrase (Just qc)
                        return $ translations <> nextPart
            otherwise ->
                error $ "Invalid HTTP response code: " ++ show (rspCode rsp)

    parseQueryContinue :: String -> Result String
    parseQueryContinue jsonString = do
        json <- decode jsonString
        queryContinue <- json ! "query-continue"
        langlinks <- queryContinue ! "langlinks"
        langlinks ! "llcontinue"


-- | Construct Wikipedia URL for given source language, phrase,
-- and an optional continuation token.
wikipediaUrl :: String -> String -> Maybe String -> URI
wikipediaUrl sourceLang phrase continue =
    fromJust $ parseURI url
  where
    url = concat ["https://"
                 , sourceLang
                 , ".wikipedia.org/w/api.php?"
                 , urlEncodeVars urlArgs
                 ]
    urlArgs = [("action", "query")
              , ("prop", "langlinks")
              , ("format", "json")
              , ("titles", phrase)
              ] ++ maybe [] (\c -> [("llcontinue", c)]) continue


-- | Parse Wikipedia response into a list of translations.
parseTranslations :: String -> IO Translations
parseTranslations jsonString =
    case readWikipediaJson jsonString of
        Ok t -> return t
        Error msg -> fail msg
  where
    readWikipediaJson jsonString = do
        json <- decode jsonString
        query <- json ! "query"
        pages <- query ! "pages"
        -- Get the only child of "pages"
        let [(_, JSObject page)] = fromJSObject pages
        langlinks <- page ! "langlinks"
        readJSON langlinks

instance JSON Translations where
    readJSON (JSArray jsonTrans) =
        Translations <$> mapM readJSONTranslation jsonTrans
      where
        readJSONTranslation (JSObject jt) =
            (,) <$> jt ! "lang" <*> jt ! "*"
    showJSON = undefined


-- Utilities

(!) :: (JSON a) => JSObject JSValue -> String -> Result a
(!) = flip valFromObj
