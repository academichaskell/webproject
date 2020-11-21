{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Character where

import Import
import Text.Lucius
--import Database.Persist.Postgresql

formCharacter :: Maybe Character -> Form Character
formCharacter character = renderDivs $ Character
    <$> areq textField (FieldSettings "Nome do personagem"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) (fmap characterName character)
    <*> areq textField (FieldSettings "Classe"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) (fmap characterClass character)
    <*> areq textField (FieldSettings "Arma primária"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) (fmap characterWeapon character)

auxCharacterR :: Route App -> Maybe Character -> Handler Html
auxCharacterR rt character = do
    (widget,_) <- generateFormPost (formCharacter character)
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/avatarRegister.lucius")
        $(whamletFile "templates/avatarRegister.hamlet")

getCharacterR :: Handler Html
getCharacterR = auxCharacterR CharacterR Nothing

postCharacterR :: Handler Html
postCharacterR = do
    ((resp,_),_) <- runFormPost (formCharacter Nothing)
    case resp of
        FormSuccess character -> do
            cid <- runDB $ insert character
            redirect ListCharactersR
        _ -> redirect HomeR
    
getListCharactersR :: Handler Html
getListCharactersR = do
    characters <- runDB $ selectList [] [Asc CharacterName]
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/listCharacters.lucius")
        $(whamletFile "templates/listCharacters.hamlet")

getUpdateCharacterR :: CharacterId -> Handler Html
getUpdateCharacterR cid = do
    old <- runDB $ get404 cid
    auxCharacterR (UpdateCharacterR cid) (Just old)

postUpdateCharacterR :: CharacterId -> Handler Html
postUpdateCharacterR cid = do
    ((resp,_),_) <- runFormPost (formCharacter Nothing)
    case resp of
        FormSuccess new -> do
            runDB $ replace cid new
            redirect (ListCharactersR)
        _ -> redirect HomeR