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

hashRate = 60e9
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

blockTime :: Difficulty -> Time
blockTime d = d * 2^32 / hashRate

revenueDeriv :: Difficulty -> Money
revenueDeriv d = btcPerBlock * btcToUSD / blockTime d

-- | How long will a difficulty stick around?
diffPeriod :: (HashRate -> Time) -- ^ Inverse integral of network hash rate
           -> Difficulty
           -> Time
diffPeriod inv d = inv $ blocksPerPeriod * 2^32 * d

-- | The lenth of each difficulty period, and the revenue gained in that period
revenuePeriods :: NHR -> INHR -> [(Time, Money)]
revenuePeriods nhr inhr = unfoldr (Just . step) $ accum 0
    where
        accum t = (difficulty $ nhr t, t)

        step (d, t) = let period = diffPeriod (inhr t) d
                      in ((period, period * revenueDeriv d), accum $ t + period)

revenue :: NHR -> INHR -> Time -> Money
revenue nhr inhr t0 = sumRevenue t0 $ revenuePeriods nhr inhr
    where
        sumRevenue t ((dt, m):rs) = if dt < t
                                    then m + sumRevenue (t - dt) rs
                                    else m * t / dt
        sumRevenue _ _ = error "Revenue ended"

income :: NHR -> INHR -> Time -> Money
income nhr inhr t = revenue nhr inhr t - cost t

cost :: Time -> Money
cost t = t * power * energyCost / kWhToJ

-- Raspberry pi
power :: Power
power = hashRate / efficiency + 5
