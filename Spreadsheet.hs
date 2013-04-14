{-# LANGUAGE RankNTypes, BangPatterns, TypeOperators, TupleSections #-}
module Main where

-- Required modules:
--
-- deepseq
-- time
-- vector
-- monad-par    - not in HP
-- netwire      - not in HP
-- monte-carlo  - not in HP

import Prelude hiding ((.), id)

import Control.DeepSeq
import Control.Exception (assert)
--import Control.Monad.Par
import Data.Time.Calendar
import qualified Data.Vector as V
import Control.Monad.MC
import Control.Wire
import Data.Summary

-- | A normal monte-carlo wire.
--
--   The units of this wire is "days". NOT seconds.
type MCWire a b = forall e. Wire e MC a b

-- | A slightly prettier version of MCWire. I hope it doesn't obscure things
--   too much...
type a ~> b = MCWire a b

-- | A monte-carlo wire which can potentially inhibit.
type MCIWire e from to = Wire e MC from to

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
globalHashRate :: a ~> Double
globalHashRate = averageHashRate >>>
    deriveExponential
        (\dHashRate userHashRate -> do
            newUsers <- poisson $ dHashRate / userHashRate
            return $ (fromIntegral newUsers) * userHashRate)
        -- values retried April 12, 2013
        (60e12, fromGregorian 2013 4 13)
        (21e12, fromGregorian 2013 2 13)

-- | Meh. Close enough. Feel free to replace it with something more accurate.
globalBTC :: a ~> Double
globalBTC = (mkFix dbtc * 144) >>> integral_ initBtc
    where
        _50Btc = fromGregorian 2009 1 1
        _25Btc = fromGregorian 2012 11 27
        _12Btc = fromGregorian 2017 10 20 -- approximately...

        -- Initial btc estimate at the origin. We assume that
        -- the origin is in the 25 btc/block region.
        initBtc = assert (origin >= _25Btc &&
                          origin <= _12Btc) $
            25*144*(fromIntegral $ origin `diffDays` _25Btc) + 10500000

        -- ten minute intervals in a day: 144
        dbtc dt _ | (round dt `addDays` origin) < _50Btc  = Right 0
                  | (round dt `addDays` origin) < _25Btc = Right 50
                  | (round dt `addDays` origin) < _12Btc = Right 25
                  | otherwise = error "Add more options to this case!"

-- | This doesn't even look exponential, but I doubt the market can sustain
--   super-exponential growth forever... right?
marketCap :: a ~> Double
marketCap = deriveSimpleExponential
    (52.7e6, fromGregorian 2012 6 1)
    (305e6, fromGregorian 2013 3 1)

-- | The USD/BTC exchange rate.
--
--   This should be log-normal! Waiting on a patch to gsl-random:
--
--   > https://github.com/patperry/hs-gsl-random/pull/4
exchangeRate :: a ~> Double
exchangeRate = marketCap / globalBTC -- I can just do that!?

-- | @see deriveExponential
--
--   Derive an exponential function with a non-random fudging function.
deriveSimpleExponential :: (Double, Day)
                        -> (Double, Day)
                        -> (a ~> Double)
deriveSimpleExponential =
    deriveExponential (\a _ -> pure a)

-- | This function is used to derive an exponential quantity whose daily rate of
--   change is some random (most likely poisson) factor times the current value
--   of the quantity at any point in time:
--
--   Let y be the quantity we are trying to predict.
--   Let dy be the daily change of that quantity.
--   Let x be the input to this wire (of type 'a').
--
--   To predict such a quantity, we need two things:
--
--     1) A function 'f' which, when given the current estimate for the change
--        in the current day and the input to this wire, produces a "fudged"
--        value. i.e., a current estimate that's the result of a random process.
--
--        'f' runs in the Monte-Carlo monad.
--
--        If you don't need this feature, use 'deriveSimpleExponential' instead.
--
--     2) Two points on the exponential curve. I suggest plotting your quantity
--        on a log-linear scale, then using a linear regression to find these
--        points. These points can be in any order.
--
--   ==========
--
--   Here's a breakdown of the math, for the special case of predicting the
--   global network hash rate:
--
--   We're using a log-lin graph, and we have two points. Therefore, we can use
--   2-point form of a line taught in elementary school:
--
--     dy = log(y2) - log(y1) = log(y2/y1)
--     dt = t2 - t1
--     m = dy/dt`
--     log(H(t)) - log(y2) = m*(t - t2)
--
--     H(t) = y2*exp(m*(t - t2))
--
--   our initial condition is at H(init_date), and our derivative each step has
--   a mean of:
--
--     dH/dt = m*H(t)
--
--   We model this process with a random variable. Given another wire
--   representing average hash rate per machine, we model the number of new
--   machines joining the network each day as a poisson distribution, with a
--   mean of:
--
--     μ = m*H(t)/average_hash_rate
--
--   finally, we multiply this random variable by the average hash rate to get
--   an estimate for the amount of hashes added to the network this step.
--
--   Huzzah!
deriveExponential :: (Double -- ^ y
                      -> a   -- ^ x
                      -> MC Double) -- ^ 'f'. See above.
                  -> (Double, Day) -- ^ (y2, t2)
                  -> (Double, Day) -- ^ (y1, t1)
                  -> (a ~> Double)
deriveExponential fudge (y2', t2') (y1', t1') =
    -- reorder the points so that t2 comes after t1.
    let (y2, t2, y1, t1) = if t1' <= t2'
                            then (y2', t2', y1', t1')
                            else (y1', t1', y2', t2')

        deltat = fromIntegral $ t2 `diffDays` t1
        dlogy = log $ y2 / y1

        exponentialFactor = dlogy / deltat

        initialValue :: Double
        initialValue = (y2 *) . exp $
            exponentialFactor * (fromIntegral $ origin `diffDays` t2)

    -- We're using euler integration here. For long prediction periods, this
    -- may be too numerically unstable. If so, consider using RK4 instead.
     in mkStateM initialValue $ \dt (x, !y) -> do
          dy <- fudge (exponentialFactor*y) x
          return (Right y, y + dy*dt)

-- | Let's just simulate a full year a million times and see what happens.
main :: IO ()
main = do print $ test (365) 1000000 exchangeRate

{-
-- | Assumes 'Summary' is a monoid... which it isn't. Yet.
--   Waiting on: https://github.com/patperry/hs-monte-carlo/pull/6
--
--   Runs a monte-carlo wire a given number of times, for a given number
--   of simulation steps.
doTrial :: Int            -- ^ The number of simulation steps to take.
        -> Int            -- ^ The number of times to simulate per step.
        -> (() ~> Double) -- ^ The wire to test.
        -> Par (V.Vector Summary)
doTrial steps trials wire =
    let range = InclusiveRange 0 (trials - 1)

        -- simulate the wire for a given number of steps, returning all the
        -- intermediate values. The result is going to be backwards so we get
        -- nice tail recursion, but this isn't really a problem. We flip it
        -- back around after the map-reduce is done.
        simulation :: MC [Double]
        simulation = do
            (_, vals) <- foldN (wire, []) $ \(w, vals) -> do
                (val, w') <- stepWire w 1 ()
                let vals' = either (const vals) (:vals) val
                return (w', vals')
            return vals

        perIndex :: Int -> Par (V.Vector Summary)
        perIndex i =
            let rng = mt19937 i
             in evalMC simulation rng
                  >>> V.fromList
                  >>> V.map (\x -> summary [x])
                  >>> return

        reducer :: V.Vector Summary -> V.Vector Summary -> Par (V.Vector Summary)
        reducer x y = return $ V.zipWith mappend x y

        base :: V.Vector Summary
        base = V.replicate steps mempty

     -- Yay. The whole thing's a map-reduce job! :)
     in V.reverse <$> parMapReduceRange range perIndex reducer base
-}

instance NFData Summary where
    rnf !_ = ()

-- | Iterate a computation a given number of times, returning the final result.
foldN :: Monad m => Int -> a -> (a -> m a) -> m a
foldN 0  x _ = return x
foldN n !x f = do y <- f x
                  foldN (n-1) y f

-- | A single-threaded version of above, until my pull request is accepted.
test :: Int -- ^ The number of simulation steps to take.
     -> Int -- ^ The number of times to simulate per step.
     -> (() ~> Double) -- ^ The wire to test.
     -> V.Vector Summary
test steps trials w =
    let -- simulate the wire for a given number of steps, returning all the
        -- intermediate hash rates. The result is backwards! We really need
        -- this for tail recursion, and Vector fusion will help make this a
        -- non-issue later.
        simulate :: MC [Double]
        simulate = do
            (_, vals) <- foldN steps (w, []) $
                \(w', vals) -> do
                    (val, w'') <- stepWire w' 1 ()
                    let vals' = either (const vals) (:vals) val
                    return (w'', vals')
            return vals

        emptySummaries = V.replicate steps (summary [])

        -- simulate the wire 'trials' times, returning summaries of the
        -- simulations at each time step.
     in flip evalMC (mt19937 0) $
          foldN trials emptySummaries $ \s -> do
            vals <- simulate
            let vvals = V.reverse $ V.fromList vals
            return $!! V.zipWith update s vvals
