{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( currentMedDay
    , MedsConfig (Config)
    ) where

import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Data.Dates
import           Data.Ini
import           Data.Text

data MedDay = Arms
            | Legs
            deriving (Eq, Show, Read)

data MedsConfig = Config { getStartDate   :: DateTime
                         , getCurrentDate :: DateTime }

configPath :: FilePath
configPath = "app\\config.ini"

config = pack "CONFIG"

startMedDay = pack "StartMedDay"

startYear = pack "StartYear"
startMonth = pack "StartMonth"
startDay = pack "StartDay"

startDate = pack "StartDate"

calcShit :: Ini -> DateTime -> Either String MedDay
calcShit ini today = do
    let get val = read . unpack <$> lookupValue config val ini
    smd <- get startMedDay
    sy <- get startYear
    sm <- get startMonth
    sd <- get startDay
    let startDay = DateTime sy sm sd 0 0 0
    let daysPassed = datesDifference today startDay
    return $ if even daysPassed then smd else flop smd
    where flop Arms = Legs
          flop Legs = Arms

currentMedDay :: ( MonadIO m
                 , MonadError String m
                 , MonadReader MedsConfig m )
                 => m MedDay
currentMedDay = do
    today <- getCurrentDate <$> ask
    (Right ini) <- liftIO $ readIniFile configPath
    either throwError return (calcShit ini today)
