{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql
import Text.Lucius

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/homepage.lucius")
        $(whamletFile "templates/homepage.hamlet")
