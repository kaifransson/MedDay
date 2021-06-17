{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}
module Meds
  ( MedDayInfo (..)
  , currentMedDay
  ) where

import           Data.Aeson          (ToJSON)
import           Data.Aeson.Deriving (FieldLabelModifier, GenericEncoded (..),
                                      SnakeCase, (:=))
import           Data.Time           (Day, diffDays)
import           GHC.Generics        (Generic)
import           Meds.App            (MedsAppT, withConfig)
import           Meds.Calendar       (MonadCalendar (..))
import           Meds.Config         (MedDay (..), MedsConfig (..))

data MedDayInfo = MedDayInfo
  { medDay     :: MedDay
  , currentDay :: Day
  }
  deriving stock (Generic)
  deriving (ToJSON) via MedsEncoding MedDayInfo

type MedsEncoding = GenericEncoded
  '[ FieldLabelModifier := SnakeCase
   ]

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
