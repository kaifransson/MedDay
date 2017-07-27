{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( currentMedDay
    , MedsConfig (Config)
    ) where

import           Control.Monad.Reader.Class
import           Data.Dates

data MedDay = Arms
            | Legs
            deriving (Eq, Show, Read)

data MedsConfig = Config { getStartDate   :: DateTime
                         , getCurrentDate :: DateTime
                         , getStartMedDay :: MedDay }

currentMedDay :: ( MonadReader MedsConfig m )
                 => m MedDay
currentMedDay = do
    config <- ask
    let startDay = getStartDate config
    let today = getCurrentDate config
    let smd = getStartMedDay config
    let daysPassed = datesDifference today startDay
    return $ if even daysPassed
                then smd
                else flop smd
    where flop Arms = Legs
          flop Legs = Arms
