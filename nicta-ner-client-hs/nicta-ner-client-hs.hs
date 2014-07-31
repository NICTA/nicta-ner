{-# LANGUAGE OverloadedStrings #-}

{-
NICTA t3as NER Client
Copyright 2014 NICTA

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/gpl-3.0.html>.
-}

--import Prelude as P
import Control.Applicative       ((<*>), (<$>))
import Control.Monad             (mzero,liftM)
import Control.Monad.IO.Class    (liftIO)
import Data.Aeson                (Value (Object), FromJSON, parseJSON, (.:)
                                 , decode)
import Data.ByteString.Lazy as L (ByteString, empty)
import Data.Maybe                (catMaybes)
import Data.Version              (showVersion)
import qualified Data.Text as T            (Text, pack, intercalate, unpack, empty, concat)
import qualified Data.Text.Lazy.IO as TIO  (readFile)
import qualified Data.Text.Lazy as LT      (pack)
import Data.Text.Lazy.Encoding as LTE (encodeUtf8)
import Network.HTTP.Conduit      (RequestBody (RequestBodyLBS), Response (..)
                                 , httpLbs, method, parseUrl, requestBody
                                 , withManager)
import Paths_nicta_ner_client_hs (version)
import System.Console.GetOpt     (ArgOrder (Permute), ArgDescr (NoArg, ReqArg)
                                 , OptDescr (Option), getOpt, usageInfo)
import System.Environment        (getProgName, getArgs)
import System.Exit               (exitSuccess)


data NerType = Person | Organization | Location | Date | Unknown deriving (Show)

data Token = Token { startIndex    :: Int
                   , text          :: T.Text
                   } deriving (Show, Read)

data Phrase = Phrase { phrase         :: [Token]
                     , phraseType     :: T.Text
                     , phrasePosition :: Int
                     , score          :: [Double]
                     } deriving (Show, Read)

data NerResponse = NerResponse  { phrases   :: [[Phrase]]
                                , tokens    :: [[Token]]
                                } deriving (Show, Read)

instance FromJSON Token where
    parseJSON (Object v) = Token <$>
                            v .: "startIndex" <*>
                            v .: "text"
    parseJSON _          = mzero

instance FromJSON Phrase where
    parseJSON (Object v) = Phrase <$>
                            v .: "phrase" <*>
                            v .: "phraseType" <*>
                            v .: "phrasePosition" <*>
                            v .: "score"
    parseJSON _          = mzero

instance FromJSON NerResponse where
    parseJSON (Object v) = NerResponse <$>
                            v .: "phrases" <*>
                            v .: "tokens"
    parseJSON _          = mzero

-- TODO: remove debug data when finished with it
test :: NerResponse
test = NerResponse {
        phrases = [[Phrase {phrase = [Token {startIndex = 0, text = "Jack"}],
                            phraseType = "PERSON",
                            phrasePosition = 0, score = [0.0,40.0,-10.0]}
                   ,Phrase {phrase = [Token {startIndex = 9, text = "Jill"}],
                            phraseType = "PERSON",
                            phrasePosition = 2, score = [0.0,40.0,-10.0]}
                  ]],
        tokens = [[Token {startIndex = 0, text = "Jack"}
                  ,Token {startIndex = 5, text = "and"}
                  ,Token {startIndex = 9, text = "Jill"}
                  ,Token {startIndex = 13, text = "."}
                 ]]
        }

responseToString :: NerResponse -> String
responseToString NerResponse {phrases = pss, tokens = tss} =
    T.unpack (foldl
        (\acc (ps, ts) -> T.concat
            [ acc
            , tokensToText ts
            , "\n===============================================\n"
            , phrasesToText ps
            ]
        )
        T.empty
        (zip pss tss)
    )

phrasesToText :: [Phrase] -> T.Text
phrasesToText ps = T.concat $ map
    (\p -> T.concat
        [ T.pack (show (phrasePosition p)), ": "
        , T.intercalate " " (map text (phrase p)), "\t"
        , phraseType p, "\t"
        , T.intercalate ", " (map (T.pack . show) (score p)), "\t"
        , "\t" -- TODO: attachedWordMap
        -- , p.phrasePosition, p.phraseStubPosition, p.phraseStubLength, p.phraseLength
        , "\n"
        ]
    )
    ps

tokensToText :: [Token] -> T.Text
tokensToText ts = T.intercalate " " (map text ts)

data Opts = Opts { usage :: Bool
                 , url   :: String
                 , txt   :: L.ByteString
                 }

defaultOpts :: Opts
defaultOpts = Opts { usage = False
                   , url   = "http://ner.t3as.org/nicta-ner-web/rest/v1.0/ner"
                   , txt   = L.empty
                   }

cmdLineArgs :: [OptDescr (Opts -> Opts)]
cmdLineArgs =
    [ Option "h" ["help"]
        (NoArg (\opts -> opts {usage = True})) "Show this help message."

    , Option [] ["url"]
        (ReqArg (\arg opts -> opts {url = arg}) "<webservice url>")
        ("The full URL to the NER web service. Default: " ++ url defaultOpts)

    , Option [] ["txt"]
        (ReqArg (\arg opts -> opts {txt = (LTE.encodeUtf8 . LT.pack) arg})
            "<text to analyse>")
        "The text to perform NER analysis on."
    ]


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    (opts, files) <- parseArgs progName args
    if usage opts || (txt opts == L.empty && null files)
        then do
            putStrLn $ usageInfo (header progName) cmdLineArgs
            exitSuccess
        else do
            results <- sequence $ runWith opts files
            mapM_ (putStrLn . responseToString) (catMaybes results)


parseArgs :: String -> [String] -> IO (Opts, [String])
parseArgs progName args =
    case getOpt Permute cmdLineArgs args of
        (opts, files, []) -> return (foldl (flip id) defaultOpts opts, files)
        (_, _, errMsgs)   -> error $ concat errMsgs
                                     ++ usageInfo (header progName) cmdLineArgs


header :: String -> String
header progName =
    "\nVersion " ++ showVersion version ++ "\n\n"
    ++ "Usage: " ++ progName ++ " [OPTIONS...] [FILES...]\n\n"
    ++ "Please pass either the --txt option, or some files with text to "
    ++ "analyse - do not pass both.\n"


runWith :: Opts -> [String] -> [IO (Maybe NerResponse)]
runWith opts files = do
    let ner = performNer $ url opts
        t = txt opts
    if t /= L.empty
        then [ner t]
        else map (>>= ner) $ readToLbs files

readToLbs :: [String] -> [IO L.ByteString]
readToLbs [] = []
-- is it really necessary to encode to UTF8?
readToLbs files = map (liftM encodeUtf8 . TIO.readFile) files


performNer :: String -> L.ByteString -> IO (Maybe NerResponse)
performNer webServiceUrl analyseTxt = withManager $ \manager -> do
    req' <- liftIO $ parseUrl webServiceUrl
    let req = req' { method = "POST", requestBody = RequestBodyLBS analyseTxt }
    res <- httpLbs req manager
    return (decode $ responseBody res :: Maybe NerResponse)


nerType :: T.Text -> NerType
nerType "PERSON"       = Person
nerType "ORGANIZATION" = Organization
nerType "LOCATION"     = Location
nerType "DATE"         = Date
nerType _              = Unknown
