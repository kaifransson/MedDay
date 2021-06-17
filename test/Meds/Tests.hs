{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Meds.Tests
  ( tests
  ) where

import           Control.Monad.Trans.Reader (Reader, ReaderT (..))
import           Data.Functor.Identity      (Identity (..))
import           Data.Time                  (Day, addDays, fromGregorian)
import           Hedgehog                   (Gen,
                                             Group (Group, groupName, groupProperties),
                                             Property, checkParallel, forAll,
                                             property, (/==), (===))
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Meds                       (MedDayInfo (..), currentMedDay)
import           Meds.App                   (runMedsAppT)
import           Meds.Calendar              (MonadCalendar (..))
import           Meds.Config                (MedDay (..), MedsConfig (..))

tests :: IO Bool
tests = checkParallel $ Group
  { groupName = "currentMedDay tests"
  , groupProperties =
    [ ("Even number of days gives identity", even_number_of_days_gives_identity)
    , ("Odd number of days gives new med day", odd_number_of_days_gives_new_med_day)
    ]
  }

even_number_of_days_gives_identity :: Property
even_number_of_days_gives_identity = property $ do
  startDate <- forAll day
  startMedDay <- forAll randomMedDay
  daysPassed <- forAll $ Gen.filter even . Gen.integral $ Range.linear 0 10_000
  let
    config = Config
      { startDate
      , startMedDay
      }
    today'sMedDay =
      medDay
      . runConstCalendar (addDays daysPassed startDate)
      . runMedsAppT config
      $ currentMedDay
  today'sMedDay === startMedDay

odd_number_of_days_gives_new_med_day :: Property
odd_number_of_days_gives_new_med_day = property $ do
  startDate <- forAll day
  startMedDay <- forAll randomMedDay
  daysPassed <- forAll $ Gen.filter odd . Gen.integral $ Range.linear 0 10_000
  let
    config = Config
      { startDate
      , startMedDay
      }
    today'sMedDay =
      medDay
      . runConstCalendar (addDays daysPassed startDate)
      . runMedsAppT config
      $ currentMedDay
  today'sMedDay /== startMedDay

randomMedDay :: Gen MedDay
randomMedDay = Gen.element [Arms, Legs]

day :: Gen Day
day = do
  year <- Gen.integral $ Range.linearFrom 2016 1970 2100
  month <- Gen.integral $ Range.linear 1 12
  day <- Gen.integral $ Range.linear 1 31
  pure $ fromGregorian year month day

newtype ConstCalendar a = ConstCalendar { unConstCalendar :: Day -> a }
  deriving
    ( Functor
    , Applicative
    , Monad
    ) via Reader Day

instance MonadCalendar ConstCalendar where
  getCurrentDate = ConstCalendar id

runConstCalendar :: Day -> ConstCalendar a -> a
runConstCalendar = flip unConstCalendar
