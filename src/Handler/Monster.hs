{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Monster where

import Import
import Text.Lucius
--import Database.Persist.Postgresql

formMonster :: Maybe Monster -> Form Monster
formMonster monster = renderDivs $ Monster
    <$> areq textField (FieldSettings "Nome do monstro"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) (fmap monsterName monster)
    <*> areq doubleField (FieldSettings "HP"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) (fmap monsterLife monster)

auxMonsterR :: Route App -> Maybe Monster -> Handler Html
auxMonsterR rt monster = do
    (widget,_) <- generateFormPost (formMonster monster)
    defaultLayout $ do
        if (rt == MonsterR) then do
            toWidgetHead $(luciusFile "templates/monsterRegister.lucius")
            $(whamletFile "templates/monsterRegister.hamlet")
        else do
            toWidgetHead $(luciusFile "templates/monsterUpdate.lucius")
            $(whamletFile "templates/monsterUpdate.hamlet")

getMonsterR :: Handler Html
getMonsterR = auxMonsterR MonsterR Nothing

postMonsterR :: Handler Html
postMonsterR = do
    ((resp,_),_) <- runFormPost (formMonster Nothing)
    case resp of
        FormSuccess monster -> do
            mid <- runDB $ insert monster
            redirect ListMonstersR
        _ -> redirect HomeR

getListMonstersR :: Handler Html
getListMonstersR = do
    monsters <- runDB $ selectList [] [Asc MonsterName]
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/listMonsters.lucius")
        $(whamletFile "templates/listMonsters.hamlet")

getUpdateMonsterR :: MonsterId -> Handler Html
getUpdateMonsterR mid = do
    old <- runDB $ get404 mid
    auxMonsterR (UpdateMonsterR mid) (Just old)

postUpdateMonsterR :: MonsterId -> Handler Html
postUpdateMonsterR mid = do
    ((resp,_),_) <- runFormPost (formMonster Nothing)
    case resp of
        FormSuccess new -> do
            runDB $ replace mid new
            redirect (ListMonstersR)  
        _ -> redirect HomeR

postDeleteMonsterR :: MonsterId -> Handler Html
postDeleteMonsterR cid = do
    _ <- runDB $ get404 cid
    runDB $ delete cid
    redirect ListMonstersR
