module Main where

import           Control.Monad.Trans.Except
import           Lib

main :: IO ()
main = do
    result <- runExceptT currentMedDay
    either oops yay result
    where oops = putStrLn
          yay = print
