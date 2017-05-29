-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import Prelude hiding (catch)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, mapM, (>=>))
import Control.Exception (catch, IOException)
import Data.Aeson hiding (defaultOptions, Options)
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Network.HTTP.Types.URI as URI
import Pipes
import qualified Pipes.Prelude as P
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = do
    argv <- getArgs
    (opts, args) <- parseCmdLineArgs argv

    when (length args < 1) $
        error "No phrase specified."
    let phrase = Text.pack $ unwords args

    TLS.setGlobalManager =<< TLS.newTlsManager
    catch (printTranslations phrase opts) handleError
  where
    printTranslations :: Text -> Options -> IO ()
    printTranslations phrase opts = runEffect $
        fetchTranslations phrase opts >-> P.print

    handleError :: IOException -> IO ()
    handleError e =
        hPutStrLn stderr $ "<Could not obtain translations: " ++ show e ++ ">"


-- Program options & command line

data Options = Options
    { optSourceLang :: Text
    , optDestLangs :: [Text]
    }

defaultOptions = Options
    { optSourceLang = "en"
    , optDestLangs = ["de", "fr", "es", "pl"]
    }

cmdLineFlags :: [OptDescr (Options -> Options)]
cmdLineFlags =
    [ Option ['s'] ["source"]
      (ReqArg (\f opts -> opts { optSourceLang = Text.pack f }) "LANG")
      "language of the source phrase"
    , Option ['d'] ["dest"]
      (ReqArg (\f opts -> opts { optDestLangs = Text.pack <$> splitOn "," f })
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
newtype Translations = Translations [(Text, Text)]
                       deriving (Monoid, Eq)

-- | Filter translations through a list of languages.
(<&>) :: Translations -> [Text] -> Translations
(Translations al) <&> list =
    Translations $ filter ((`elem` list) . fst) al

instance Show Translations where
    show (Translations ts) = intercalate "\n" $ map (uncurry showOne) ts
      where
        showOne lang t = Text.unpack lang ++ ": " ++ Text.unpack t


-- | Produces translations of given phrase.
-- Results are streamed incrementally since Wikipedia splits the response
-- over multiple pages of separate HTTP responses.
fetchTranslations :: MonadIO m => Text -> Options -> Producer Translations m ()
fetchTranslations phrase Options{..} =
    fetchTranslationsPart phrase Nothing
  where
    fetchTranslationsPart :: MonadIO m => Text -> Maybe String -> Producer Translations m ()
    fetchTranslationsPart phrase continue = do
        let url = wikipediaUrl optSourceLang phrase continue
        (ts, continue) <- liftIO $ handleWikipediaResponse =<< fetchUrl url
        when (ts /= mempty) $
            yield ts
        case continue of
            Nothing -> return ()
            Just c -> fetchTranslationsPart phrase (Just c)

    -- | Process response from Wikipedia, returning translations + continuation token.
    handleWikipediaResponse :: HTTP.Response LB.ByteString -> IO (Translations, Maybe String)
    handleWikipediaResponse response =
        case status of
            s | s >= 200, s < 299 -> do
                translations <- either fail return $ parseTranslations body
                let filtered = translations <&> optDestLangs
                let qc = parseQueryContinue body
                return (filtered, qc)
            s -> error $ "Invalid HTTP response code: " ++ show s
      where
        body = HTTP.responseBody response
        status = statusCode . HTTP.responseStatus $ response

    parseQueryContinue :: LB.ByteString -> Maybe String
    parseQueryContinue jsonBody =
        decode' jsonBody >>= parseMaybe parse
      where
        parse = withObject "continuation token" $
            (.: "continue") >=> (.: "llcontinue")


-- | Construct Wikipedia URL for given source language, phrase,
-- and an optional continuation token.
wikipediaUrl :: Text -> Text -> Maybe String -> String
wikipediaUrl sourceLang phrase continue = Text.unpack $ Text.concat [
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
              ] ++ maybe [] (\c -> [("llcontinue", Text.pack c)]) continue
    urlEncodeVars =
        Text.intercalate "&" . map (\(k, v) -> k <> "=" <> urlEncode v)
    urlEncode = Text.decodeUtf8 . URI.urlEncode True . Text.encodeUtf8


-- | Parse Wikipedia response into a list of translations.
parseTranslations :: LB.ByteString -> Either String Translations
parseTranslations jsonString =
    eitherDecode' jsonString >>= parseEither parse
  where
    parse :: Value -> Parser Translations
    parse = withObject "Wikipedia translations" $ \o -> do
        (Object pages) <- o .: "query" >>= (.: "pages")
        -- Get the only child of `pages` object
        let [(_, (Object page))] = HM.toList pages
        page .: "langlinks"

instance FromJSON Translations where
    parseJSON = withArray "langlinks" $ \ts -> do
        Translations . filter (/= ("", "")) <$> mapM parse (V.toList ts)
      where
        parse :: Value -> Parser (Text, Text)
        parse = withObject "single translation" $ \t -> do
            lang <- t .: "lang"
            translation <- t .: "*"
            return (Text.strip lang, Text.strip translation)


-- Utility functions

-- | Fetch given URL and return final HTTP response, after any redirects.
fetchUrl :: String -> IO (HTTP.Response LB.ByteString)
fetchUrl url = do
    request <- HTTP.parseRequest url
    manager <- TLS.getGlobalManager
    response <- HTTP.httpLbs request manager
    return response
