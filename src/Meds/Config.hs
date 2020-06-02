{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Meds.Config
  ( MedsConfig(..)
  , MedDay(..)
  ) where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Time (Day)
import           GHC.Generics (Generic)

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
