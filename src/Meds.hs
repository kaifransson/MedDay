module Meds
  ( currentMedDay
  ) where

import           Data.Time   (Day, diffDays)
import           Meds.App    (MedsAppT, withConfig)
import           Meds.Config (MedDay (..), MedsConfig (..))

currentMedDay :: Monad m => Day -> MedsAppT m MedDay
currentMedDay today = withConfig $ \config -> do
  let
    startDay = startDate config
    smd = startMedDay config
    daysPassed = diffDays today startDay
  pure $ if even daysPassed
                then smd
                else flop smd
  where
    flop Arms = Legs
    flop Legs = Arms
