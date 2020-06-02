{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Meds.Main
  ( main
  ) where

import           Control.Monad.Except (MonadError, liftEither, runExceptT)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (runReaderT)
import           Data.Time (UTCTime(..), getCurrentTime, fromGregorian)
import           Data.Functor (void)
import           Data.Ini (Ini, readIniFile, lookupValue)
import           Data.Text (Text, unpack)
import           Meds (MedsConfig, MedsConfig(..), currentMedDay)
import           Paths_Meds (getDataFileName)

configg :: Text
configg = "CONFIG" -- name shadowing happened so I should figure out a better name
startYear :: Text
startYear = "StartYear"
startMonth :: Text
startMonth = "StartMonth"
startDay :: Text
startDay = "StartDay"
startMedDay :: Text
startMedDay = "StartMedDay"

readIni ::
     MonadIO m
  => MonadError String m
  => m Ini
readIni = do
    configPath <- liftIO $ getDataFileName "config.ini"
    ini <- liftIO $ readIniFile configPath
    liftEither ini

loadConfig ::
     MonadIO m
  => MonadError String m
  => m MedsConfig
loadConfig = do
  ini <- readIni
  let get key = liftEither $ read . unpack <$> lookupValue configg key ini
  sy <- get startYear
  sm <- get startMonth
  sd <- get startDay
  let sdt = fromGregorian sy sm sd
  smd <- get startMedDay
  today <- utctDay <$> liftIO getCurrentTime
  return $ Config sdt today smd

main :: IO ()
main = do
  (Right config) <- runExceptT loadConfig
  result <- runExceptT $ runReaderT currentMedDay config
  either oops yay result
  void getLine
  where
    oops = putStrLn
    yay = print
