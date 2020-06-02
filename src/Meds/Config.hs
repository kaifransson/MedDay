{-# LANGUAGE DerivingStrategies #-}
module Meds.Config
  ( MedsConfig(..)
  , MedDay(..)
  ) where

import           Data.Dates (DateTime)

data MedsConfig = Config
  { getStartDate   :: DateTime
  , getCurrentDate :: DateTime
  , getStartMedDay :: MedDay
  }
  deriving stock (Show, Eq)

data MedDay
  = Arms
  | Legs
  deriving stock (Eq, Show, Read)
