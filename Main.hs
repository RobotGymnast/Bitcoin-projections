module Main ( main ) where

import Prelude hiding ((.), id)

import Control.Monad.Par

import MCWire
import Spreadsheet

-- | Let's just simulate a full year a million times and see what happens.
main :: IO ()
main = do
    print . runPar $ simulate 365 1000000 exchangeRate
