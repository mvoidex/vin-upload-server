{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}
module Main where

import Prelude hiding (log)

import Control.Monad.IO.Class
import Control.Exception (bracket)
import Data.Lens.Template
import Data.ByteString (ByteString)

import qualified Database.Redis as Redis

import Snap.Core
import Snap.Http.Server

import Snap.Snaplet
import Snap.Snaplet.Vin
import Snap.Snaplet.SimpleLog
import Snap.Snaplet.RedisDB

-- | Main application
data App = App {
    _appLog :: Snaplet SimpleLog,
    _redis :: Snaplet RedisDB,
    _vin :: Snaplet Vin }

type AppHandler = Handler App App

makeLens ''App

instance MonadLog (Handler App App) where
    askLog = with appLog askLog

-- | Log configuration
logConfig :: Rules
logConfig = []

-- | Main application initializer
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
    l <- liftIO $ newLog defaultPolitics logConfig [
        logger text (file "log/app.log"),
        logger text console]

    lg <- nestSnaplet "applog" appLog (simpleLogInit_ l)
    pg <- nestSnaplet "redis" redis (redisDBInit Redis.defaultConnectInfo)
    v <- nestSnaplet "vin" vin vinInit

    addRoutes routes

    return $ App lg pg v

main :: IO ()
main = bracket (runSnaplet appInit) clean start where
    clean ~(_, _, c) = c
    start (msgs, site, _) = do
        quickHttpServe site

{-
site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")
-}

-- | Routes
routes :: [(ByteString, AppHandler ())]
routes = [
    ("foo", writeBS "bar"),
    ("echo/:echoparam", echoHandler)]

echoHandler :: AppHandler ()
echoHandler = scope "echo" $ do
    log Info "test info"
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL") writeBS param
