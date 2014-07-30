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

import Control.Applicative       ((<*>), (<$>))
import Control.Monad             (mzero,liftM)
import Control.Monad.IO.Class    (liftIO)
import Data.Aeson                (Value (Object), FromJSON, parseJSON, (.:)
                                 , decode)
import Data.ByteString.Lazy as L (ByteString, empty)
import Data.Maybe                (catMaybes)
import Data.Version              (showVersion)
import Data.Text.Lazy as T       (Text, pack)
import Data.Text.Lazy.IO as TIO  (readFile)
import Data.Text.Lazy.Encoding   (encodeUtf8)
import Network.HTTP.Conduit      (RequestBody (RequestBodyLBS), Response (..)
                                 , httpLbs, method, parseUrl, requestBody
                                 , withManager)
import Paths_nicta_ner_client_hs (version)
import System.Console.GetOpt     (ArgOrder (Permute), ArgDescr (NoArg, ReqArg)
                                 , OptDescr (Option), getOpt, usageInfo)
import System.Environment        (getProgName, getArgs)
import System.Exit               (exitSuccess)


cmdLineArgs :: [OptDescr (Opts -> Opts)]
cmdLineArgs =
    [ Option "h" ["help"]
        (NoArg (\opts -> opts {usage = True})) "Show this help message."

    , Option [] ["url"]
        (ReqArg (\arg opts -> opts {url = arg}) "<webservice url>")
        ("The full URL to the NER web service. Default: " ++ url defaultOpts)

    , Option [] ["txt"]
        (ReqArg (\arg opts -> opts {txt = (encodeUtf8 . T.pack) arg})
            "<text to analyse>")
        "The text to perform NER analysis on."
    ]

data Opts = Opts { usage :: Bool
                 , url   :: String
                 , txt   :: L.ByteString
                 }

defaultOpts :: Opts
defaultOpts = Opts { usage = False
                   , url   = "http://ner.t3as.org/nicta-ner-web/rest/v1.0/ner"
                   , txt   = L.empty
                   }


data NerType = Person | Organization | Location | Date | Unknown deriving (Show)

type StartIndex = Int
data NerEntity = NerEntity T.Text NerType StartIndex

data Token = Token  { startIndex    :: Int
                    , text          :: T.Text
                    } deriving (Show)

data Phrase = Phrase    { phrase        :: [Token]
                        , phraseType    :: T.Text
                        , score         :: [Double]
                        } deriving (Show)

data NerResponse = NerResponse  { phrases   :: [[Phrase]]
                                , tokens    :: [[Token]]
                                } deriving (Show)

instance FromJSON Token where
    parseJSON (Object v) = Token <$>
                            v .: "startIndex" <*>
                            v .: "text"
    parseJSON _          = mzero

instance FromJSON Phrase where
    parseJSON (Object v) = Phrase <$>
                            v .: "phrase" <*>
                            v .: "phraseType" <*>
                            v .: "score"
    parseJSON _          = mzero

instance FromJSON NerResponse where
    parseJSON (Object v) = NerResponse <$>
                            v .: "phrases" <*>
                            v .: "tokens"
    parseJSON _          = mzero


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    (opts, files) <- parseArgs progName args
    if usage opts
        then do
            putStrLn $ usageInfo (header progName) cmdLineArgs
            exitSuccess
        else do
            results <- sequence $ runWith opts files
            mapM_ print $ catMaybes results


parseArgs :: String -> [String] -> IO (Opts, [String])
parseArgs progName args =
    case getOpt Permute cmdLineArgs args of
        (opts, files, []) -> return (foldl (flip id) defaultOpts opts, files)
        (_, _, errMsgs)   -> error $ concat errMsgs
                                     ++ usageInfo (header progName) cmdLineArgs


header :: String -> String
header progName =
    "Version " ++ showVersion version ++ "\n\n"
    ++ "Usage: " ++ progName ++ " [OPTIONS...] [FILES...]\n\n"
    ++ "Please pass either the --txt option or some files with text to analyse."


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
