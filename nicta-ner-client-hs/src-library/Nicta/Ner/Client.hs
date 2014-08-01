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

module Nicta.Ner.Client where

import Control.Applicative      ((<*>), (<$>))
import Control.Monad            (mzero)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (Value (Object), FromJSON, parseJSON, (.:)
                                , decode)
import Data.ByteString.Lazy as L (ByteString)
import qualified Data.Map as M  (Map, lookup)
import Data.Maybe               (fromMaybe)
import qualified Data.Text as T (Text, pack, intercalate, unpack, empty, concat)
import Network.HTTP.Conduit     (RequestBody (RequestBodyLBS) , Response (..)
                                , httpLbs, method, requestBody, parseUrl
                                , withManager)


data NerType = Person | Organization | Location | Date | Unknown deriving (Show)

data Token = Token { startIndex    :: Int
                   , text          :: T.Text
                   } deriving (Show)

data Phrase = Phrase { phrase             :: [Token]
                     , phraseType         :: T.Text
                     , phrasePosition     :: Int
                     , phraseStubPosition :: Int
                     , phraseStubLength   :: Int
                     , phraseLength       :: Int
                     , attachedWordMap    :: M.Map T.Text T.Text
                     , score              :: [Double]
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
                            v .: "phrasePosition" <*>
                            v .: "phraseStubPosition" <*>
                            v .: "phraseStubLength" <*>
                            v .: "phraseLength" <*>
                            v .: "attachedWordMap" <*>
                            v .: "score"
    parseJSON _          = mzero

instance FromJSON NerResponse where
    parseJSON (Object v) = NerResponse <$>
                            v .: "phrases" <*>
                            v .: "tokens"
    parseJSON _          = mzero

data Opts = Opts { usage :: Bool
                 , url   :: String
                 , txt   :: L.ByteString
                 }


performNer :: String -> L.ByteString -> IO (Maybe NerResponse)
performNer webServiceUrl analyseTxt = withManager $ \manager -> do
    req' <- liftIO $ parseUrl webServiceUrl
    let req = req' { method = "POST", requestBody = RequestBodyLBS analyseTxt }
    res <- httpLbs req manager
    return (decode $ responseBody res :: Maybe NerResponse)


responseToString :: NerResponse -> String
responseToString NerResponse {phrases = pss, tokens = tss} =
    T.unpack (foldl
        (\acc (ps, ts) -> T.concat
            [ acc
            , tokensToText ts
            , "\n===============================================\n"
            , phrasesToText ps
            , "\n"
            ]
        )
        T.empty
        (zip pss tss)
    )


phrasesToText :: [Phrase] -> T.Text
phrasesToText = T.concat . map
    -- the text we want to generate
    -- 0: John  PERSON  11.25, 40.0, -10.0  null    0:0:0:1:1
    (\p -> T.concat
        -- 0: John\t
        [ t $ phrasePosition p, ": "
        , tokensToText (phrase p), "\t"
        -- PERSON\t
        , phraseType p, "\t"
        -- 11.25, 40.0, -10.0\t
        , T.intercalate ", " (map (T.pack . show) (score p)), "\t"
        -- null\t  this is a bad example...
        , fromMaybe "null" (M.lookup "prep" (attachedWordMap p)), "\t"
        -- 0:0:1:1\n
        , t $ startIndex (head (phrase p)), ":"
        , t $ phrasePosition p, ":"
        , t $ phraseStubPosition p, ":"
        , t $ phraseStubLength p, ":"
        , t $ phraseLength p, "\n"
        ]
    )
    where
        t = T.pack . show


tokensToText :: [Token] -> T.Text
tokensToText ts = T.intercalate " " (map text ts)


nerType :: T.Text -> NerType
nerType "PERSON"       = Person
nerType "ORGANIZATION" = Organization
nerType "LOCATION"     = Location
nerType "DATE"         = Date
nerType _              = Unknown
