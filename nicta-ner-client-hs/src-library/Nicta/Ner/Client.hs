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
import Data.ByteString          (ByteString)
import qualified Data.Map as M  (Map, lookup, toList)
import Data.Maybe               (fromMaybe)
import qualified Data.Text as T (Text, pack, intercalate, unpack, empty, concat)
import Network.HTTP.Conduit     (RequestBody (RequestBodyBS) , Response (..)
                                , httpLbs, method, requestBody, parseUrl
                                , withManager)
import Network.HTTP.Types       (urlEncode)


-- hardcoded entity class types, maybe this should handle arbitrary classes too?
data NerType = Person | Organization | Location | Date | Ethnic | Unknown
               deriving (Show, Eq, Ord)

data Token = Token { startIndex    :: Int
                   , text          :: T.Text
                   } deriving (Show)

data PhraseType = PhraseType { entityClass :: T.Text } deriving (Show)

data Phrase = Phrase { phrase             :: [Token]
                     , phraseType         :: PhraseType
                     , phrasePosition     :: Int
                     , phraseStubPosition :: Int
                     , phraseStubLength   :: Int
                     , phraseLength       :: Int
                     , attachedWordMap    :: M.Map T.Text T.Text
                     , score              :: M.Map T.Text Double
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

instance FromJSON PhraseType where
    parseJSON (Object v) = PhraseType <$>
                            v .: "entityClass"
    parseJSON _          = mzero

data Opts = Opts { usage :: Bool
                 , url   :: String
                 , txt   :: ByteString
                 }


performNer :: String -> ByteString -> IO (Maybe NerResponse)
performNer webServiceUrl analyseTxt = withManager $ \manager -> do
    req' <- liftIO $ parseUrl webServiceUrl
    let encodedTxt = urlEncode True analyseTxt
        req = req' { method = "POST", requestBody = RequestBodyBS encodedTxt }
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
    -- 0: Jack  PERSON  LOCATION:6.0, PERSON:7.5, ORGANIZATION:7.0, ETHNIC:0.0  null    0:0:0:1:1
    (\p -> T.concat
        -- 0: Jack\t
        [ t $ phrasePosition p, ": "
        , tokensToText (phrase p), "\t"
        -- PERSON\t
        , (entityClass . phraseType) p, "\t"
        -- LOCATION:6.0, PERSON:7.5, ORGANIZATION:7.0, ETHNIC:0.0\t
        --, T.intercalate ", " (map (T.pack . show) (score p)), "\t"
        , T.intercalate ", "
            (map (\(c, s) -> T.concat [c, ":", (T.pack . show) s])
                (M.toList $ score p))
        , "\t"
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
nerType "ETHNIC"       = Ethnic
nerType _              = Unknown
