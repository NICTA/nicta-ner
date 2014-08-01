{-# LANGUAGE OverloadedStrings #-}

{-
NICTA t3as NER Web UI
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

module Handler.Ner where

import           Import
import qualified Data.List               as L   (concat, length, head)
import           Data.Maybe                     (fromJust, fromMaybe)
import qualified Data.Text               as T   (intercalate)
import qualified Data.Text.Lazy          as LT  (fromStrict)
import qualified Data.Text.Lazy.Encoding as LTE (encodeUtf8)
import           Nicta.Ner.Client


type Prep = Text
type Scores = Text
type StartIndex = Int

data NerEntity = NerEntity Text NerType Prep Scores StartIndex


getNerR :: Handler Html
getNerR = do
    let nerText = "" :: Text
        nerResponse = []
    defaultLayout $(widgetFile "ner")


postNerR :: Handler Html
postNerR = do
    raw <- runInputPost $ ireq textareaField "nerText"
    let nerText = unTextarea raw
    response <- liftIO $ analyse nerText
    let nerResponse = toNerEntities $ fromJust response
    defaultLayout $(widgetFile "ner")


analyse :: Text -> IO (Maybe NerResponse)
analyse t = do
    let encoded = (LTE.encodeUtf8 . LT.fromStrict) t
    performNer "http://localhost:8080/nicta-ner-web/rest/v1.0/ner" encoded


toNerEntities :: NerResponse -> [NerEntity]
toNerEntities r = map toEntity (L.concat (phrases r))


toEntity :: Phrase -> NerEntity
toEntity p = NerEntity t typ prep scr idx
    where
        t    = tokensToText (phrase p)
        typ  = nerType (phraseType p)
        prep = fromMaybe "" (lookup "prep" (attachedWordMap p))
        scr  = T.intercalate ", " (map (pack . show) (score p))
        idx  = startIndex (L.head (phrase p))
