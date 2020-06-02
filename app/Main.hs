{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Dates
import           Data.Ini
import           Data.Text
import           Lib
import           Paths_Meds

configg = "CONFIG" -- name shadowing happened so I should figure out a better name

startYear = "StartYear"
startMonth = "StartMonth"
startDay = "StartDay"

startMedDay = "StartMedDay"

readIni :: ( MonadIO m
           , MonadError String m )
           => m Ini
readIni = do
    configPath <- liftIO $ getDataFileName "config.ini"
    ini <- liftIO $ readIniFile configPath
    liftEither ini

loadConfig :: ( MonadIO m
              , MonadError String m )
              => m MedsConfig
loadConfig = do
    ini <- readIni
    let get key = liftEither $ read . unpack <$> lookupValue configg key ini
    sy <- get startYear
    sm <- get startMonth
    sd <- get startDay
    let sdt = DateTime sy sm sd 0 0 0
    smd <- get startMedDay
    today <- liftIO getCurrentDateTime
    return $ Config sdt today smd

main :: IO ()
main = do
    (Right config) <- runExceptT loadConfig
    result <- runExceptT $ runReaderT currentMedDay config
    either oops yay result
    void getLine
    where oops = putStrLn
          yay = print
