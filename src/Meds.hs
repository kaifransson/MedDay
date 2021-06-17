{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
module Meds
  ( MedDayInfo (..)
  , currentMedDay
  ) where

import           Data.Aeson    (ToJSON)
import           Data.Time     (Day, diffDays)
import           GHC.Generics  (Generic)
import           Meds.App      (MedsAppT, withConfig)
import           Meds.Calendar (MonadCalendar (..))
import           Meds.Config   (MedDay (..), MedsConfig (..))

data MedDayInfo = MedDayInfo
  { medDay     :: MedDay
  , currentDay :: Day
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

currentMedDay :: MonadCalendar m => MedsAppT m MedDayInfo
currentMedDay = withConfig $ \config -> do
  today <- getCurrentDate
  let
    startDay = startDate config
    smd = startMedDay config
    daysPassed = diffDays today startDay
    medDay = if even daysPassed
             then smd
             else flop smd
  pure MedDayInfo
    { medDay
    , currentDay = today
    }
  where
    flop Arms = Legs
    flop Legs = Arms
