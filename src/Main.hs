{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}
module Main where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (bracket)
import Data.Maybe
import Data.Lens.Template
import Data.ByteString (ByteString)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Database.Redis as Redis

import System.FilePath
import System.Directory

import Snap.Core
import Snap.Http.Server

import Snap.Snaplet
import Snap.Util.FileUploads
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

-- | Main application initializer
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
    l <- liftIO $ newLog (fileCfg "log.cfg" 10) [
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
    start (_, site, _) = do
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
    ("/upload",     method POST $ vinUploadData),
    ("/state",      method GET  $ vinStateRead),
    ("/state",      method POST $ vinStateRemove)]

vinUploadData :: AppHandler ()
vinUploadData = scope "upload" $ do
    log Trace "Uploading data"
    tmp <- liftIO getTemporaryDirectory
    fs <- handleFileUploads tmp policy (part policy) (handleUploads "uploaded")
    forM_ fs $ \f -> do
        log Trace $ T.concat ["Uploaded to file: ", T.pack f]
        prog <- getParam "program"
        case prog of
            Nothing -> log Error "Program not specified"
            Just p -> do
                log Info $ T.concat ["Uploading ", T.pack f]
                log Trace $ T.concat ["Program: ", T.decodeUtf8 p]
                log Trace $ T.concat ["Initializing state for file: ", T.pack f]
                with vin $ initUploadState f
                log Trace $ T.concat ["Uploading data from file: ", T.pack f]
                with vin $ uploadData (T.unpack . T.decodeUtf8 $ p) ("uploaded" </> f)
    where
        handleUploads uploadPath = liftIO . liftM catMaybes . mapM handleUpload where
            handleUpload (_, Left _) = return Nothing
            handleUpload (info, Right f) = do
                createDirectoryIfMissing True uploadPath
                copyFile f $ uploadPath </> fname
                return $ Just fname
                where
                    fname = maybe (error "File name not specified") (T.unpack . T.decodeUtf8) $ partFileName info
        policy = foldr (.) id policies defaultUploadPolicy where
            policies = [
                setProcessFormInputs True,
                setMaximumFormInputSize (100 * 1024),
                setMinimumUploadRate (1000 * 1024),
                setMinimumUploadSeconds 10,
                setUploadTimeout 20]
        part pol _ = allowWithMaximumSize $ getMaximumFormInputSize pol

vinStateRead :: AppHandler ()
vinStateRead = scope "state" $ scope "get" $ do
    log Trace "Getting state"
    with vin getState

vinStateRemove :: AppHandler ()
vinStateRemove = scope "state" $ scope "remove" $ do
    log Trace "Remove alert by id"
    res <- getParam "id"
    log Trace $ T.concat ["id: ", maybe "<null>" (T.pack . show) res]
    with vin removeAlert

echoHandler :: AppHandler ()
echoHandler = scope "echo" $ do
    log Info "test info"
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL") writeBS param
