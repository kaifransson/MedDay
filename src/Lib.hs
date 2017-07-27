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

calcShit :: MedsConfig -> MedDay
calcShit config = let startDay = getStartDate config
                      today = getCurrentDate config
                      smd = getStartMedDay config
                      daysPassed = datesDifference today startDay
                   in if even daysPassed
                        then smd
                        else flop smd
                where flop Arms = Legs
                      flop Legs = Arms

currentMedDay :: ( MonadReader MedsConfig m )
                 => m MedDay
currentMedDay = do
    config <- ask
    return $ calcShit config
