{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Dates
import           Data.Ini
import           Data.Text
import           Lib
import           Paths_Meds

configg = pack "CONFIG" -- name shadowing happened so I should figure out a better name

startYear = pack "StartYear"
startMonth = pack "StartMonth"
startDay = pack "StartDay"

startMedDay = pack "StartMedDay"

readIni :: ( MonadIO m
           , MonadError String m )
           => m Ini
readIni = do
    configPath <- liftIO $ getDataFileName "config.ini"
    ini <- liftIO $ readIniFile configPath
    liftEither ini

liftEither :: ( Monad m -- Data.Ini insists on returning concrete Either types
              , MonadError e m )
              => Either e r -> m r
liftEither = either throwError return

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
