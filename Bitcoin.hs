module Bitcoin where

import Data.Fixed
import Data.List
import Num.Fraction

type BaseType = Fraction Milli
type HashRate = BaseType
type Difficulty = BaseType
type Time = BaseType
type Money = BaseType
type Power = BaseType

type NHR = Time -> HashRate
-- | Inverse integral of the network hash rate
type INHR = HashRate -> Time -> HashRate

-- H / J
efficiency = 1e9
btcPerBlock = 25
btcToUSD = 17.64
energyCost = 0.10
kWhToJ = 3600e3
netTimePerBlock = 600
blocksPerPeriod = 2016

difficulty :: HashRate -> Difficulty
difficulty nhr = nhr * netTimePerBlock / 2^32

blockTime :: HashRate -> Difficulty -> Time
blockTime h d = d * 2^32 / h

revenueDeriv :: HashRate -> Difficulty -> Money
revenueDeriv h d = btcPerBlock * btcToUSD / blockTime h d

-- | How long will a difficulty stick around?
diffPeriod :: (HashRate -> Time) -- ^ Inverse integral of network hash rate
           -> Difficulty
           -> Time
diffPeriod inv d = inv $ blocksPerPeriod * 2^32 * d

-- | The lenth of each difficulty period, and the income in that period
incomePeriods :: NHR -> INHR -> HashRate -> [(Time, Money)]
incomePeriods nhr inhr h = unfoldr (Just . step) $ accum 0
    where
        accum t = (difficulty $ nhr t, t)

        step (d, t) = let period = diffPeriod (inhr t) d
                      in ((period, period * (revenueDeriv h d - costDeriv h)), accum $ t + period)

income :: NHR -> INHR -> HashRate -> Time -> Money
income nhr inhr h t0 = sumIncome t0 $ incomePeriods nhr inhr h
    where
        sumIncome t ((dt, m):rs) = if dt < t
                                   then m + sumIncome (t - dt) rs
                                   else m * t / dt
        sumIncome _ _ = error "income ended"

costDeriv :: HashRate -> Money
costDeriv h = power h * energyCost / kWhToJ

-- Raspberry pi + ASICs
power :: HashRate -> Power
power h = h / efficiency + 5
