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

import Import

getNerR :: Handler Html
getNerR = do
    let text = "" :: Text
    defaultLayout $(widgetFile "ner")

postNerR :: Handler Html
postNerR = do
        text <- runInputPost $ ireq textField "ner"
        defaultLayout $(widgetFile "ner")
