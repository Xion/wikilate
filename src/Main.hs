-- Wikilate.hs
-- Translates given phrase using different language versions of Wikipedia

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import Prelude hiding (catch)

import Control.Monad (when, mapM, (>=>))
import Control.Exception (catch, IOException)
import Data.Aeson hiding (Options)
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Network.HTTP.Types.URI as URI
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as OA
import Pipes
import qualified Pipes.Prelude as P
import System.IO (hPutStrLn, stderr)
import Text.Regex.TDFA


main :: IO ()
main = do
    opts <- customExecParser parserPrefs commandLine
    TLS.setGlobalManager =<< TLS.newTlsManager
    catch (printTranslations opts) handleError
  where
    parserPrefs = defaultPrefs { prefDisambiguate = True }

    printTranslations :: Options -> IO ()
    printTranslations opts = runEffect $ fetchTranslations opts >-> P.print

    handleError :: IOException -> IO ()
    handleError e =
        hPutStrLn stderr $ "<Could not obtain translations: " ++ show e ++ ">"


-- Program options & command line

data Options = Options
    { optSourceLang :: Text
    , optDestLangs :: [Text]
    , optPhrase :: Text
    }

-- | Full parse for the command line, with proper help display.
commandLine :: OA.ParserInfo Options
commandLine = info (options <**> helper)
    ( fullDesc
    <> progDesc "Translate the PHRASE using Wikipedia"
    <> header "wikilate -- Wikipedia-based translator"
    <> failureCode 2)

-- | Parser for command line options, written in the optparse-applicative style.
options :: OA.Parser Options
options = pure Options
    <*> option str
        ( long "source"
        <> short 's'
        <> value "en"  -- default
        <> metavar "LANG"
        <> showDefault
        <> help "Source language of the phrase")
    <*> option csv
        ( long "dest"
        <> short 'd'
        <> value ["de", "fr", "es", "pl"]  -- default
        <> metavar "LANG[,LANG[,...]]"
        <> showDefaultWith (Text.unpack . Text.intercalate ",")
        <> help "Languages to translate to")
    <*> argument str
        ( completer (mkCompleter completePhrase)
        <> metavar "PHRASE"
        <> help "Phrase to translate")
  where
    -- Custom argument readers (converters)
    csv = OA.maybeReader $ Just . map Text.strip . (Text.splitOn ",") . Text.pack


-- | Argument completion for PHRASE.
-- Looks up Wikipedia for the list of articles starting with given prefix.
completePhrase :: String -> IO [String]
completePhrase partial = do
    let
      -- TODO: get the actual sourceLang here somehow
      url = wikipediaPrefixSearchUrl "en" (Text.pack partial)
    response <- fetchUrl url
    if is2xx (status response) then
      do
        let body = HTTP.responseBody response
        return $ map Text.unpack $ cleanupResults . parseResults $ body
      else return [partial]
  where
    status = statusCode . HTTP.responseStatus
    is2xx s = 200 <= s && s < 299

    parseResults :: LB.ByteString -> [Text]
    parseResults jsonBody =
        let results = fromMaybe [] $ decode' jsonBody >>= parseMaybe parse
        in map Text.pack results
      where
        parse = withObject "search results" $
            (.: "query") >=> (.: "prefixsearch") >=> mapM (.: "title")

    cleanupResults = map cleanup
      where
        cleanup r = Text.pack $ subRegex trailingParenRegex (Text.unpack r) ""
        trailingParenRegex = mkRegex "[[:space:]]+\\([^)]+\\)$" -- " (bar)" in "Foo (bar)"

        -- Below is just some boilerplate to make regex-tdfa expose a sane interface.

        mkRegex :: String -> Regex
        mkRegex s = make s
          where
            make :: RegexMaker Regex CompOption ExecOption String => String -> Regex
            make = makeRegex

        subRegex :: Regex -> String -> String -> String
        subRegex re input repl = subs input
          where
            subs s = fromMaybe s $ do
                (before, match, after) <- matchM re s :: Maybe (String, String, String)
                when (null match) $
                    error $ "Internal error in (subRegex" ++ "<regex>" ++ " " ++ s ++ ")"
                return $ before ++ repl ++ subs after


-- | URL to Wikipedia API endpoint for performing prefix searches.
wikipediaPrefixSearchUrl :: Text -> Text -> String
wikipediaPrefixSearchUrl sourceLang prefix = Text.unpack $ Text.concat
    [ "https://"
    , sourceLang
    , ".wikipedia.org/w/api.php?"
    , mkQueryString urlArgs ]
  where
    urlArgs = [("action", "query")
              , ("list", "prefixsearch")
              , ("format", "json")
              , ("pssearch", prefix)
              , ("pslimit", Text.pack . show $ maxResults)]
    maxResults = 20


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
fetchTranslations :: MonadIO m => Options -> Producer Translations m ()
fetchTranslations Options{..} =
    fetchTranslationsPart Nothing
  where
    fetchTranslationsPart :: MonadIO m => Maybe String -> Producer Translations m ()
    fetchTranslationsPart continue = do
        let url = wikipediaLangLinksUrl optSourceLang optPhrase continue
        response <- liftIO $ fetchUrl url
        (ts, continue) <- either fail return $ handleWikipediaResponse response
        when (ts /= mempty) $
            yield ts
        case continue of
            Nothing -> return ()
            Just c -> fetchTranslationsPart (Just c)

    -- | Process response from Wikipedia, returning translations + continuation token.
    handleWikipediaResponse :: HTTP.Response LB.ByteString
                            -> Either String (Translations, Maybe String)
    handleWikipediaResponse response =
        case status of
            s | s >= 200, s < 299 -> do
                translations <- parseTranslations body
                let filtered = translations <&> optDestLangs
                let qc = parseQueryContinue body
                Right (filtered, qc)
            s -> Left $ "Invalid HTTP response code: " ++ show s
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
wikipediaLangLinksUrl :: Text -> Text -> Maybe String -> String
wikipediaLangLinksUrl sourceLang phrase continue = Text.unpack $ Text.concat [
    "https://"
    , sourceLang
    , ".wikipedia.org/w/api.php?"
    , mkQueryString urlArgs
    ]
  where
    urlArgs = [("action", "query")
              , ("prop", "langlinks")
              , ("format", "json")
              , ("titles", phrase)
              ] ++ maybe [] (\c -> [("llcontinue", Text.pack c)]) continue


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

-- | URL-encodes a list of key-value pairs to make a query string.
mkQueryString :: [(Text, Text)] -> Text
mkQueryString = Text.intercalate "&" . map (\(k, v) -> k <> "=" <> encode v)
  where
    encode = Text.decodeUtf8 . URI.urlEncode True . Text.encodeUtf8
