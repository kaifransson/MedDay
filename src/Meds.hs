{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
module Meds
  ( MedDayInfo (..)
  , currentMedDay
  ) where

import           Data.Aeson    (ToJSON)
import           Data.Time     (diffDays)
import           GHC.Generics  (Generic)
import           Meds.App      (MedsAppT, withConfig)
import           Meds.Calendar (MonadCalendar (..))
import           Meds.Config   (MedDay (..), MedsConfig (..))

newtype MedDayInfo = MedDayInfo
  { medDay :: MedDay
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
    }
  where
    flop Arms = Legs
    flop Legs = Arms
