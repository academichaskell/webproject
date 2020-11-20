{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Character where

import Import
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
        [whamlet|
            <div class="container">
                <div id="previous-btn">
                    <a class="back-home" href=@{HomeR}>
                        <img src=@{StaticR img_arrowLeft_svg}>
                <form action=@{CharacterR} method=post>
                    <h1>
                        Criação de personagem
                    ^{widget}
                    <input class="submit-btn" type="submit" value="Criar personagem">
        |]

postCharacterR :: Handler Html
postCharacterR = do
    ((resp,_),_) <- runFormPost formCharacter
    case resp of
        FormSuccess character -> do
            cid <- runDB $ insert character
            redirect HomeR
        _ -> redirect HomeR
    