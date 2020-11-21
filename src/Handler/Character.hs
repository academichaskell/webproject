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

formCharacter :: Form Character
formCharacter = renderDivs $ Character
    <$> areq textField (FieldSettings "Nome do personagem"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) Nothing
    <*> areq textField (FieldSettings "Classe"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) Nothing
    <*> areq textField (FieldSettings "Arma primária"
                                      Nothing
                                      Nothing
                                      Nothing
                                      [("class", "text-field")]
                       ) Nothing

getCharacterR :: Handler Html
getCharacterR = do
    (widget,_) <- generateFormPost formCharacter
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/avatarRegister.lucius")
        $(whamletFile "templates/avatarRegister.hamlet")

postCharacterR :: Handler Html
postCharacterR = do
    ((resp,_),_) <- runFormPost formCharacter
    case resp of
        FormSuccess character -> do
            cid <- runDB $ insert character
            redirect HomeR
        _ -> redirect HomeR
    
getListCharactersR :: Handler Html
getListCharactersR = do
    characters <- runDB $ selectList [] [Asc CharacterName]
    defaultLayout $ do
        $(whamletFile "templates/listCharacters.hamlet")