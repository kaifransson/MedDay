{-# LANGUAGE LambdaCase #-}
module Meds.Main
  ( main
  ) where

import           Data.Aeson                 (eitherDecodeFileStrict, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor               (void)
import           Meds                       (currentMedDay)
import           Meds.App                   (runMedsAppT)
import           Meds.Calendar              (runIOCalendarT)
import           Meds.Config                (MedsConfig)
import           Paths_Meds                 (getDataFileName)
import           System.Exit                (exitFailure)

loadConfig :: IO (Either String MedsConfig)
loadConfig =
  getDataFileName "config.json" >>= eitherDecodeFileStrict

main :: IO ()
main = do
  config <- loadConfig >>= \case
    Left err -> do
      putStrLn "Failed to deserialize config"
      putStrLn err
      exitFailure
    Right cfg -> pure cfg

  (runIOCalendarT . runMedsAppT config) currentMedDay >>= LBS.putStrLn . encode
  void getLine
