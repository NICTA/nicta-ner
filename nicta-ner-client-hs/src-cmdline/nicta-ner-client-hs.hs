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

import Nicta.Ner.Client

import Control.Monad            (liftM)
import Data.ByteString          (ByteString, empty)
import Data.Maybe               (catMaybes)
import Data.Version             (showVersion)
import qualified Data.Text.IO as TIO  (readFile)
import qualified Data.Text as T (pack)
import Data.Text.Encoding as TE (encodeUtf8)
import Paths_nicta_ner_client_hs (version)
import System.Console.GetOpt    (ArgOrder (Permute), ArgDescr (NoArg, ReqArg)
                                , OptDescr (Option), getOpt, usageInfo)
import System.Environment       (getProgName, getArgs)
import System.Exit              (exitSuccess)


defaultOpts :: Opts
defaultOpts = Opts { usage = False
                   , url   = "http://ner.t3as.org/nicta-ner-web/rest/v1.0/ner"
                   , txt   = empty
                   }


cmdLineArgs :: [OptDescr (Opts -> Opts)]
cmdLineArgs =
    [ Option "h" ["help"]
        (NoArg (\opts -> opts {usage = True})) "Show this help message."

    , Option [] ["url"]
        (ReqArg (\arg opts -> opts {url = arg}) "<webservice url>")
        ("The full URL to the NER web service. Default: " ++ url defaultOpts)

    , Option [] ["txt"]
        (ReqArg (\arg opts -> opts {txt = (TE.encodeUtf8 . T.pack) arg})
            "<text to analyse>")
        "The text to perform NER analysis on."
    ]


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    (opts, files) <- parseArgs progName args
    if usage opts || (txt opts == empty && null files)
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
    if t /= empty
        then [ner t]
        else map (>>= ner) $ readToLbs files


readToLbs :: [String] -> [IO ByteString]
readToLbs [] = []
-- is it really necessary to encode to UTF8?
readToLbs files = map (liftM encodeUtf8 . TIO.readFile) files
