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

import Control.Applicative     ((<*>), (<$>))
import Control.Monad           (mzero)
import Control.Monad.IO.Class  (liftIO)
import Data.Aeson              (Value (Object), FromJSON, parseJSON, (.:)
                               , decode)
import Data.ByteString.Lazy    (ByteString)
import Data.Text               (Text)
import Network.HTTP.Conduit    (RequestBody (RequestBodyLBS), Response (..)
                               , httpLbs, method, parseUrl, requestBody
                               , withManager)

data NerType = Person | Organization | Location | Date | Unknown
               deriving (Show)

type StartIndex = Int
data NerEntity = NerEntity Text NerType StartIndex

data Token = Token  { startIndex    :: Int
                    , text          :: Text
                    } deriving (Show)

data Phrase = Phrase    { phrase        :: [Token]
                        , phraseType    :: Text
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
    ner <- performNer "http://localhost:8080/nicta-ner-web/rest/v1.0/ner" "Jack and Jill"
    case ner of
        Nothing -> print "No Named-Entities found."
        Just a  -> print a

type WebServiceUrl = String
performNer :: WebServiceUrl -> ByteString -> IO (Maybe NerResponse)
performNer url txt = withManager $ \manager -> do
    req' <- liftIO $ parseUrl url
    let req = req' { method = "POST", requestBody = RequestBodyLBS txt }
    res <- httpLbs req manager
    return (decode $ responseBody res :: Maybe NerResponse)

nerType :: Text -> NerType
nerType "PERSON"       = Person
nerType "ORGANIZATION" = Organization
nerType "LOCATION"     = Location
nerType "DATE"         = Date
nerType _              = Unknown
