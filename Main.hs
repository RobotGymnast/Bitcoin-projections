module Main ( main ) where

import MCWire
import Spreadsheet

-- | Let's just simulate a full year a million times and see what happens.
main :: IO ()
main = do
    print $ simulate 365 1000000 exchangeRate
