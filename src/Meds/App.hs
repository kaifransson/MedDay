{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
module Meds.App
  ( MedsAppT
  , runMedsAppT
  , withConfig
  , runMedsApp
  ) where

import           Control.Monad.Identity (Identity (runIdentity))
import           Control.Monad.Reader   (ReaderT (..), ask)
import           Control.Natural        (type (~>))
import           Meds.Config            (MedsConfig)

newtype MedsAppT m a = MedsAppT { unMedsAppT :: ReaderT MedsConfig m a }
  deriving newtype (Functor, Applicative, Monad)

type MedsApp = MedsAppT Identity

runMedsAppT :: MedsConfig -> MedsAppT m ~> m
runMedsAppT config = flip runReaderT config . unMedsAppT

runMedsApp :: MedsConfig -> MedsApp a -> a
runMedsApp config = runIdentity . runMedsAppT config

withConfig ::
     Monad m
  => (MedsConfig -> MedsAppT m a)
  -> MedsAppT m a
withConfig go = MedsAppT $ do
  config <- ask
  unMedsAppT $ go config
