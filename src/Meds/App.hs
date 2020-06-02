{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
module Meds.App
  ( MedsAppT
  , runMedsAppT
  , withConfig
  ) where

import           Control.Monad.Reader (ReaderT(..), ask)
import           Control.Natural (type (~>))
import           Meds.Config (MedsConfig)

newtype MedsAppT m a = MedsAppT { unMedsAppT :: ReaderT MedsConfig m a }
  deriving newtype (Functor, Applicative, Monad)

runMedsAppT :: MedsConfig -> MedsAppT m ~> m
runMedsAppT config = flip runReaderT config . unMedsAppT

withConfig ::
     Monad m
  => (MedsConfig -> MedsAppT m a)
  -> MedsAppT m a
withConfig go = MedsAppT $ do
  config <- ask
  unMedsAppT $ go config
