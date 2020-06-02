{-# LANGUAGE FlexibleContexts      #-}
module Meds
  ( currentMedDay
  ) where

import           Control.Monad.Reader (MonadReader(..))
import           Data.Time (Day, diffDays)
import           Meds.Config (MedsConfig(..), MedDay(..))

currentMedDay ::
     MonadReader MedsConfig m
  => Day -> m MedDay
currentMedDay today = do
    config <- ask
    let startDay = startDate config
    let smd = startMedDay config
    let daysPassed = diffDays today startDay
    return $ if even daysPassed
                then smd
                else flop smd
    where flop Arms = Legs
          flop Legs = Arms
