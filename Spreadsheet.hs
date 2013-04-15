{-# LANGUAGE TypeOperators, RankNTypes, TypeOperators #-}
module Spreadsheet where

import Control.Exception (assert)

import MCWire

type a ~> b = MCWire a b

type HashRate = Double

-- | A constant for now, but I'm sure that it's more interesting than that.
--   It's most likely something kind of logistic.
averageHashRate :: a ~> HashRate
averageHashRate = 300e6

-- | The starting date of our simulation.
origin :: Day
origin = fromGregorian 2013 4 12

-- | A prediction for the global hash rate of the bitcoin network.
--
--   This is based on the theory that the new miners joining the network form
--   a poisson distribution. We derive the number of new users from the
--   'averageHashRate' arrow and the current hash rate.
--
--   This quantity excludes BFL's shipments! This is almost definitely a false
--   assumption. TODO.
baseHashRate :: a ~> Double
baseHashRate = averageHashRate >>>
    deriveExponential
        (\dHashRate userHashRate -> do
            newUsers <- poisson $ dHashRate / userHashRate
            return $ (fromIntegral newUsers) * userHashRate)
        -- values retried April 12, 2013
        (60e12, fromGregorian 2013 4 13)
        (21e12, fromGregorian 2013 2 13)
        origin

-- | TODO
bflHashRate :: a ~> Double
bflHashRate = 1.44e15

netHashRate :: a ~> Double
netHashRate = baseHashRate + bflHashRate

difficulty :: a ~> Double
difficulty = netHashRate * 600 * 1e9 / 2^(32 :: Int)

-- | % of hashes due to butterfly labs.
hashesDueToBFL :: a ~> Double
hashesDueToBFL = bflHashRate / netHashRate

-- thresholds for various block rewards.
_50Btc, _25Btc, _12Btc :: Day
_50Btc = fromGregorian 2009 1 1
_25Btc = fromGregorian 2012 11 27
_12Btc = fromGregorian 2016 10 20 -- approximately...

-- | How many bitcoins are there in every block mined?
--
--   Close enough. Feel free to replace it with something more accurate.
btcPerBlock :: a ~> Double
btcPerBlock =
    let go dt _ =
          let day = round dt `addDays` origin
           in if       day < _50Btc then Right 0
               else if day < _25Btc then Right 50
               else if day < _12Btc then Right 25
               else error "Add more block thresholds!"
     in mkFix go

-- Initial world btc estimate at the origin. We assume that the origin is in
-- the 25 btc/block region.
initialBtc :: Double
initialBtc =
    assert (origin >= _25Btc &&
            origin <= _12Btc) $
      25*blocksPerDay*(fromIntegral $ origin `diffDays` _25Btc) + 10500000

blocksPerDay :: Double
blocksPerDay = 144

globalBtc :: a ~> Double
globalBtc = btcPerBlock >>> integral_ initialBtc

-- | This doesn't even look exponential, but I doubt the market can sustain
--   super-exponential growth forever... right?
marketCap :: a ~> Double
marketCap = deriveSimpleExponential
    (52.7e6, fromGregorian 2012 6 1)
    (305e6, fromGregorian 2013 3 1)
    origin

-- | The USD/BTC exchange rate.
--
--   This should be log-normal! Waiting on a patch to gsl-random:
--
--   > https://github.com/patperry/hs-gsl-random/pull/4
exchangeRate :: a ~> Double
exchangeRate = marketCap / globalBtc -- I can just do that!?
