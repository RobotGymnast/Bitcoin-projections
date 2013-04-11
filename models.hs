{-# LANGUAGE NoImplicitPrelude
           #-}

import Prelewd

import Impure

import Data.Fixed
import Data.Ratio
import Num.Fraction
import Storage.List

import Bitcoin

baseHashRate :: HashRate
baseHashRate = 47930589482844 -- April 4/13

monthHashGain :: HashRate
monthHashGain = 16e12 * 365.24 / 12

secondsPerMonth :: Time
secondsPerMonth = 3600 * 24 * 365.24 / 12

lg :: Integer -> Integer -> Integer
lg _ 1 = 0
lg b n = 1 + lg b (div n b)

isqrt :: Integer -> Integer
isqrt 0 = 0
isqrt n = isqrt' $ max 1 $ 2 * isqrt (div n 4)
    where
        isqrt' g = let f = div n g
                   in if' (abs (f - g) > 1) isqrt' $ div (f + g) 2

fsqrt :: HasResolution a => Fraction (Fixed a) -> Fraction (Fixed a)
fsqrt x = fromRational $ ((%) `on` isqrt) <$> numerator <*> denominator $ toRational x

linearNHR, constantNHR :: NHR
linearINHR, constantINHR :: INHR

linearNHR t = baseHashRate + t * monthHashGain / secondsPerMonth
linearINHR t r = let c = secondsPerMonth / monthHashGain
                     b = c * baseHashRate
                 in fsqrt (t * b * 2 + r * c * 2 + t^2 + b^2) - b - t

constantNHR _ = baseHashRate
constantINHR _ = (/ baseHashRate)
