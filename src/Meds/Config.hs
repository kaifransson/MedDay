{-# LANGUAGE DerivingStrategies #-}
module Meds.Config
  ( MedsConfig(..)
  , MedDay(..)
  ) where

import           Data.Time (Day)

data MedsConfig = Config
  { getStartDate   :: Day
  , getCurrentDate :: Day
  , getStartMedDay :: MedDay
  }
  deriving stock (Show, Eq)

data MedDay
  = Arms
  | Legs
  deriving stock (Eq, Show, Read)
