module Main where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Lib

main :: IO ()
main = do
    result <- runExceptT currentMedDay
    either oops yay result
    void getLine
    where oops = putStrLn
          yay = print
