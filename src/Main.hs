-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Text.JSON
import System.Environment (getArgs)
import System.Console.GetOpt
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Network.HTTP.Types.URI as URI


{-# ANN module "HLint: ignore Use string literal" #-}


main :: IO ()
main = do
    argv <- getArgs
    (opts, args) <- parseCmdLineArgs argv

    when (length args < 1) $
        error "No phrase specified."
    let phrase = unwords args

    TLS.setGlobalManager =<< TLS.newTlsManager
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


-- | Holds translations as an association list of (language, text),
newtype Translations = Translations [(String, String)]
                       deriving (Monoid, Eq)

-- | Filter translations through a list of languages.
(<&>) :: Translations -> [String] -> Translations
(Translations al) <&> list =
    Translations $ filter ((`elem` list) . fst) al

instance Show Translations where
    show (Translations transAL) =
        unlines $ map (\(lang, tr) -> lang ++ ": " ++ tr) transAL


-- | Retrieves translations of given phrase.
fetchTranslations :: String -> Options -> IO Translations
fetchTranslations phrase Options{..} =
    fetchTranslationsPart phrase Nothing
  where
    fetchTranslationsPart :: String -> Maybe String -> IO Translations
    fetchTranslationsPart phrase continue = do
        let url = wikipediaUrl optSourceLang phrase continue
        handleWikipediaResponse =<< fetchUrl url

    -- | Fetch given URL and return final HTTP response, after any redirects.
    fetchUrl :: String -> IO (HTTP.Response String)
    fetchUrl url = do
        request <- HTTP.parseRequest url
        manager <- TLS.getGlobalManager
        response <- HTTP.httpLbs request manager
        -- TODO: switch to using Text everywhere
        return $ (LT.unpack . LT.decodeUtf8) <$> response

    handleWikipediaResponse :: HTTP.Response String -> IO Translations
    handleWikipediaResponse response =
        case status of
            s | s >= 200, s < 299 -> do
                translations <- parseTranslations body
                let filtered = translations <&> optDestLangs
                -- TODO: this is of course a fugly side effect;
                -- make the entire thing into a pipe or something, with print as a step
                when (filtered /= mempty) $
                    print filtered
                case parseQueryContinue body of
                    Error _ -> return translations
                    Ok qc -> do
                        nextPart <- fetchTranslationsPart phrase (Just qc)
                        return $ translations <> nextPart
            otherwise ->
                error $ "Invalid HTTP response code: " ++ show status
      where
        body = HTTP.responseBody response
        status = statusCode . HTTP.responseStatus $ response

    parseQueryContinue :: String -> Result String
    parseQueryContinue jsonString =
        decode jsonString >>= (! "continue") >>= (! "llcontinue")


-- | Construct Wikipedia URL for given source language, phrase,
-- and an optional continuation token.
wikipediaUrl :: String -> String -> Maybe String -> String
wikipediaUrl sourceLang phrase continue = concat [
    "https://"
    , sourceLang
    , ".wikipedia.org/w/api.php?"
    , urlEncodeVars urlArgs
    ]
  where
    urlArgs = [("action", "query")
              , ("prop", "langlinks")
              , ("format", "json")
              , ("titles", phrase)
              ] ++ maybe [] (\c -> [("llcontinue", c)]) continue
    -- TODO: use better way of URL building than this conversion dance
    urlEncodeVars = intercalate "&" . map (\(k, v) -> k <> "=" <> urlEncode v)
    urlEncode = Text.unpack . Text.decodeUtf8
                . URI.urlEncode True
                . Text.encodeUtf8 . Text.pack


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
