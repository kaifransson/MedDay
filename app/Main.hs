{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           Data.Aeson                 (eitherDecodeStrict)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.FileEmbed             (embedFile)
import           Data.Functor               (void)
import           Meds                       (currentMedDay)
import           Meds.App                   (runMedsAppT)
import           Meds.Calendar              (runIOCalendarT)
import           Meds.Config                (MedsConfig)
import           System.Exit                (exitFailure)

loadConfig :: IO MedsConfig
loadConfig =
  case eitherDecodeStrict $(embedFile "config.json") of
    Left err -> do
      putStrLn "Failed to deserialize config"
      putStrLn err
      exitFailure
    Right cfg -> pure cfg

main :: IO ()
main = do
  config <- loadConfig
  output <- runIOCalendarT . runMedsAppT config $ currentMedDay
  LBS.putStrLn . encodePretty $ output
  void getLine
