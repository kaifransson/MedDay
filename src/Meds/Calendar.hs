{-# LANGUAGE DerivingVia #-}
module Meds.Calendar
  ( MonadCalendar (..)
  , IOCalendarT
  , runIOCalendarT
  , IOCalendar
  , runIOCalendar
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Identity (Identity (..), IdentityT (..))
import           Control.Monad.Trans    (MonadTrans (..))
import           Data.Time              (Day, UTCTime (..), getCurrentTime)
import           Meds.App               (MedsAppT)

class Monad m => MonadCalendar m where
  getCurrentDate :: m Day

instance MonadCalendar m => MonadCalendar (MedsAppT m) where
  getCurrentDate = lift getCurrentDate

newtype IOCalendarT m a = IOCalendarT { runIOCalendarT :: m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    ) via IdentityT m
  deriving
    ( MonadTrans
    ) via IdentityT

type IOCalendar = IOCalendarT Identity

runIOCalendar :: IOCalendar a -> a
runIOCalendar = runIdentity . runIOCalendarT

instance MonadIO m => MonadCalendar (IOCalendarT m) where
  getCurrentDate = liftIO $ utctDay <$> getCurrentTime
