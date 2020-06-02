module Meds.Main
  ( main
  ) where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader (runReaderT)
import           Data.Time (UTCTime(..), getCurrentTime)
import           Data.Functor (void)
import           Meds (currentMedDay)
import           Meds.Config (loadConfig)

main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  let
    getResult = runExceptT $ do
      config <- loadConfig
      runReaderT (currentMedDay today) config
  getResult >>= either oops yay
  void getLine
  where
    oops = putStrLn
    yay = print
