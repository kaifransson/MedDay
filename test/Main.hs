module Main
  ( main
  ) where

import           Hedgehog.Main (defaultMain)

import           Meds.Tests    (tests)

main :: IO ()
main = defaultMain [tests]
