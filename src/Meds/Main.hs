{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
module Meds.Main
  ( main
  ) where

import           Data.Aeson (eitherDecodeFileStrict)
import           Data.Time (UTCTime(..), getCurrentTime)
import           Data.Functor (void)
import           Meds (currentMedDay)
import           Meds.App (runMedsAppT)
import           Meds.Config (MedsConfig)
import           System.Exit (exitFailure)
import           Paths_Meds (getDataFileName)

loadConfig :: IO (Either String MedsConfig)
loadConfig =
  getDataFileName "config.json" >>= eitherDecodeFileStrict

main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  config <- loadConfig >>= \case
    Left err -> do
      putStrLn "Failed to deserialize config"
      putStrLn err
      exitFailure
    Right cfg -> pure cfg

  runMedsAppT config (currentMedDay today) >>= print
  void getLine
