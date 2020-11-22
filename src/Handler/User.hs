{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import
import Text.Lucius

formUser :: Form (User, Text)
formUser = renderDivs $ (,)
    <$> (User
        <$> areq emailField (FieldSettings "E-mail"
                                Nothing
                                Nothing
                                Nothing
                                [("class", "text-field")]
                            ) Nothing
        <*> areq passwordField (FieldSettings "Senha"
                                    Nothing
                                    Nothing
                                    Nothing
                                    [("class", "text-field")]
                                ) Nothing)
    <*> areq passwordField (FieldSettings "Confirmar senha"
                                Nothing
                                Nothing
                                Nothing
                                [("class", "text-field")]
                            ) Nothing
                            
getUserR :: Handler Html
getUserR = do
    (widget,_) <- generateFormPost formUser
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/userRegister.lucius")
        $(whamletFile "templates/userRegister.hamlet")

postUserR :: Handler Html
postUserR = do 
    ((result,_),_) <- runFormPost formUser
    case result of
        FormSuccess (user, verify) -> do
            if (userPassword user == verify) then do    
                runDB $ insert user
                setMessage [shamlet|
                    <div>
                        Usuário cadastrado com sucesso!
                |]
                redirect UserR
            else do
                setMessage [shamlet|
                    <div>
                        E-mail ou senha incorreto(s)!
                |]
                redirect UserR
        _-> redirect HomeR
