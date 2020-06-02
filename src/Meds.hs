{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Meds
  ( currentMedDay
  , MedsConfig(..)
  ) where

import           Control.Monad.Reader (MonadReader(..))
import           Data.Time (diffDays)
import           Meds.Config (MedsConfig(..), MedDay(..))

currentMedDay ::
     MonadReader MedsConfig m
  => m MedDay
currentMedDay = do
    config <- ask
    let startDay = getStartDate config
    let today = getCurrentDate config
    let smd = getStartMedDay config
    let daysPassed = diffDays today startDay
    return $ if even daysPassed
                then smd
                else flop smd
    where flop Arms = Legs
          flop Legs = Arms
