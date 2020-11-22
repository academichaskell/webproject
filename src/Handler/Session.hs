{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Session where

import Import
import Text.Lucius

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,)
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
                        ) Nothing

getLogonR :: Handler Html
getLogonR = do 
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/signin.lucius")
        $(whamletFile "templates/signin.hamlet")

postLogonR :: Handler Html
postLogonR = do 
    ((result,_),_) <- runFormPost formLogin
    case result of 
        FormSuccess (email,password) -> do 
           user <- runDB $ getBy (UniqueEmail email)
           case user of 
                Nothing -> do 
                    setMessage [shamlet|
                        <div>
                            E-mail não encontrado!
                    |]
                    redirect LogonR
                Just (Entity _ usr) -> do 
                    if (userPassword usr == password) then do
                        setSession "_EMAIL" (userEmail usr)
                        redirect ListCharactersR
                    else do 
                        setMessage [shamlet|
                            <div>
                                Senha incorreta!
                        |]
                        redirect LogonR 
        _ -> redirect HomeR
        
postLogoffR :: Handler Html 
postLogoffR = do 
    deleteSession "_EMAIL"
    redirect HomeR