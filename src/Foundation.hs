{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    authRoute _ = Just LogonR
    
    isAuthorized HomeR _ = return Authorized
    isAuthorized UserR _ = return Authorized
    isAuthorized LogonR _ = return Authorized
    isAuthorized _ _ = isUser
    makeLogger = return . appLogger

isUser :: Handler AuthResult
isUser = do 
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized
        
type Form a = Html -> MForm Handler (FormResult a, Widget)    
    
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
