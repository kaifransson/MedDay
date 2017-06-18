module Main where

import           Lib
import           Control.Monad.Trans.Except

main :: IO ()
main = do
    result <- runExceptT currentMedDay
    either oops yay result
    where oops = putStrLn
          yay = print