module Main (main) where

import Test.HUnit
import TestWinning

main :: IO Counts
main = runTestTT tests