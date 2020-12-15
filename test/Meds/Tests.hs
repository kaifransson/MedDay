{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Meds.Tests
  ( main
  ) where

import           Control.Applicative            (Applicative (liftA2))
import           Data.Time
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import           Hedgehog
import qualified Hedgehog.Gen                   as Gen
import           Hedgehog.Main
import qualified Hedgehog.Range                 as Range
import           Meds                           (currentMedDay)
import           Meds.App                       (runMedsApp)
import           Meds.Config                    (MedDay (..), MedsConfig (..))

main :: IO ()
main = defaultMain . pure . checkParallel $ Group
  { groupName = "currentMedDay tests"
  , groupProperties =
    [ ("Even number of days gives identity", even_number_of_days_gives_identity)
    , ("Odd number of days gives new med day", odd_number_of_days_gives_new_med_day)
    ]
  }

even_number_of_days_gives_identity :: Property
even_number_of_days_gives_identity = property $ do
  startDate <- forAll day
  startMedDay <- forAll medDay
  daysPassed <- forAll $ Gen.filter even . Gen.integral $ Range.linear 0 10_000
  let
    config = Config
      { startDate
      , startMedDay
      }
    today'sMedDay = runMedsApp config $ currentMedDay (addDays daysPassed startDate)
  today'sMedDay === startMedDay

odd_number_of_days_gives_new_med_day :: Property
odd_number_of_days_gives_new_med_day = property $ do
  startDate <- forAll day
  startMedDay <- forAll medDay
  daysPassed <- forAll $ Gen.filter odd . Gen.integral $ Range.linear 0 10_000
  let
    config = Config
      { startDate
      , startMedDay
      }
    today'sMedDay = runMedsApp config $ currentMedDay (addDays daysPassed startDate)
  today'sMedDay /== startMedDay

medDay :: Gen MedDay
medDay = Gen.element [Arms, Legs]

day :: Gen Day
day = do
  year <- Gen.integral $ Range.linearFrom 2016 1970 2100
  month <- Gen.integral $ Range.linear 1 12
  day <- Gen.integral $ Range.linear 1 31
  pure $ fromGregorian year month day
