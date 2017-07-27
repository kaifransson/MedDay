module Main where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Dates
import           Lib

main :: IO ()
main = do
    today <- getCurrentDateTime
    let config = Config undefined today
    result <- runExceptT $ runReaderT currentMedDay config
    either oops yay result
    void getLine
    where oops = putStrLn
          yay = print
