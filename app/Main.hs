{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Dates
import           Data.Ini
import           Data.Text
import           Lib

configPath :: FilePath
configPath = "app\\config.ini"

configg = pack "CONFIG"

startYear = pack "StartYear"
startMonth = pack "StartMonth"
startDay = pack "StartDay"

startMedDay = pack "StartMedDay"

readIni :: IO Ini
readIni = do
    (Right ini) <- readIniFile configPath
    return ini

liftEither :: ( Monad m
              , MonadError e m )
              => Either e r -> m r
liftEither = either throwError return

loadConfig :: ( MonadIO m
              , MonadError String m )
              => m MedsConfig
loadConfig = do
    ini <- liftIO readIni
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
