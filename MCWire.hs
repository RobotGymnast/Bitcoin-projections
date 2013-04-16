{-# LANGUAGE RankNTypes, BangPatterns, TypeOperators, TupleSections #-}
module MCWire ( module Control.Monad.MC
              , module Control.Wire
              , MCWire
              , MCIWire
              , module Data.Time.Calendar
              , deriveSimpleExponential
              , deriveExponential
              , module Data.Summary
              , simulate
              ) where

import Prelude hiding ((.), id)

import Control.DeepSeq
import Control.Monad.MC hiding ( sample )
import Control.Monad.Par
import Control.Wire

import Data.Monoid
import Data.Summary
import Data.Time.Calendar
import qualified Data.Vector as V

-- | A normal monte-carlo wire.
--
--   The units of this wire is "days". NOT seconds.
type MCWire a b = forall e. Wire e MC a b

type a ~> b = MCWire a b

-- | A monte-carlo wire which can potentially inhibit.
type MCIWire e from to = Wire e MC from to

-- | @see deriveExponential
--
--   Derive an exponential function with a non-random fudging function.
deriveSimpleExponential :: (Double, Day)
                        -> (Double, Day)
                        -> Day
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
                  -> Day -- ^ The origin. i.e. the day we start simulating from.
                  -> (a ~> Double)
deriveExponential fudge (y2', t2') (y1', t1') origin =
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

-- | Assumes 'Summary' is a monoid... which it isn't. Yet.
--   Waiting on: https://github.com/patperry/hs-monte-carlo/pull/6
--
--   Runs a monte-carlo wire a given number of times, for a given number
--   of simulation steps.
simulate :: Int            -- ^ The number of simulation steps to take.
        -> Int            -- ^ The number of times to simulate per step.
        -> (() ~> Double) -- ^ The wire to test.
        -> Par (V.Vector Summary)
simulate steps trials wire =
    let range = InclusiveRange 0 (trials - 1)

        -- simulate the wire for a given number of steps, returning all the
        -- intermediate values. The result is going to be backwards so we get
        -- nice tail recursion, but this isn't really a problem. We flip it
        -- back around after the map-reduce is done.
        simulation :: MC [Double]
        simulation = do
            (_, vals) <- foldN steps (wire, []) $ \(w, vals) -> do
                (val, w') <- stepWire w 1 ()
                let vals' = either (const vals) (:vals) val
                return (w', vals')
            return vals

        perIndex :: Int -> Par (V.Vector Summary)
        perIndex i =
            let rng = mt19937 (fromIntegral i)
                evaled = V.fromList $ evalMC simulation rng
                summs = V.map (\x -> summary [x]) evaled
             in return $!! summs

        reducer :: V.Vector Summary -> V.Vector Summary -> Par (V.Vector Summary)
        reducer x y = return $!! V.zipWith mappend x y

        base :: V.Vector Summary
        base = V.replicate steps mempty

     -- Yay. The whole thing's a map-reduce job! :)
     in V.reverse <$> parMapReduceRange range perIndex reducer base

-- | Iterate a computation a given number of times, returning the final result.
foldN :: Monad m => Int -> a -> (a -> m a) -> m a
foldN 0  x _ = return x
foldN n !x f = do y <- f x
                  foldN (n-1) y f
