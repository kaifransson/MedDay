{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Meds.Config
  ( MedsConfig(..)
  , MedDay(..)
  , loadConfig
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Except (MonadError, liftEither)
import           Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict)
import           Data.Time (Day)
import           GHC.Generics (Generic)
import           Paths_Meds (getDataFileName)

data MedsConfig = Config
  { startDate   :: Day
  , startMedDay :: MedDay
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data MedDay
  = Arms
  | Legs
  deriving stock (Generic, Eq, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

loadConfig ::
     MonadError String m
  => MonadIO m
  => m MedsConfig
loadConfig = do
  configPath <- liftIO $ getDataFileName "config.json"
  config <- liftIO $ eitherDecodeFileStrict configPath
  liftEither config
