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
