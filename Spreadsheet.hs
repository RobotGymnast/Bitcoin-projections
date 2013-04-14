{-# LANGUAGE RankNTypes, BangPatterns, TypeOperators #-}
module Main where

-- Required modules:
--
-- mtl
-- deepseq
-- time
-- vector
-- netwire      - not in HP
-- monte-carlo  - not in HP

import Prelude hiding ((.), id)

import Control.DeepSeq
import Control.Exception (assert)
import Control.Monad.Par
import Data.Functor
import Data.Time.Calendar
import qualified Data.Vector as V
import Control.Monad.MC
import Control.Monad.Trans
import Control.Wire
import Data.Summary
import Text.Printf

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
exchangeRate = (/) <$> marketCap <*> globalBTC

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
--        in the current time step and the input to this wire, produces a
--        "fudged" value. i.e., one that's the result of a random process.
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

        dt = fromIntegral $ t2 `diffDays` t1
        dlogy = log $ y2 / y1

        exponent = dlogy / dt

        initialValue :: Double
        initialValue = (y2 *) . exp $
            exponent * (fromIntegral $ origin `diffDays` t2)

    -- We're using euler integration here. For long prediction periods, this
    -- may be too numerically unstable. If so, consider using RK4 instead.
     in mkStateM initialValue $ \dt (x, !y) -> do
          dy <- fudge (exponent*y*dt) x
          return (Right y, y + dy)

-- | Let's just simulate a full year a million times and see what happens.
main :: IO ()
main = do print $ test 1 1000000 globalBTC
          print $ test 1 1000000 marketCap
          print $ test 1 1000000 exchangeRate

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
doTrial steps trials w =
    let range = InclusiveRange 0 (trials - 1)

        simulation :: MC [Double]
        simulation = do
            -- prime the RNG, since we seed it with _very_ predictable values.
            _ <- uniform 0 0
            sim w steps []

        perIndex :: Int -> V.Vector Summary
        perIndex i = let rng = mt19937 i
                      in evalMC simulation rng
                          >>> V.fromList
                          >>> V.map (\x -> summary [x])
                          >>> return

        reducer :: V.Vector Summary -> V.Vector Summary -> V.Vector Summary
        reducer x y = return $ V.zipWith mappend x y

        base :: V.Vector Summary
        base = V.replicate steps mempty
     -- Yay. The whole thing's a map-reduce job! :)
     in parMapReduceRange range perIndex reducer base
    where
        -- simulate the wire for a given number of steps, returning all the
        -- intermediate values. The result is backwards! We really need
        -- this for tail recursion, and Vector fusion will help make this a
        -- non-issue later.
        --sim :: MCWire () Double -> Int -> MC [Double] <- valid, but GHC bitches.
        sim _  0 vals = return vals
        sim w' k vals = do
            (val, w'') <- stepWire w' 1 ()
            let val' = either (const []) id val
            sim w'' (k-1) (val':vals)
-}

instance NFData Summary where
    rnf !s = ()

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
        --sim :: (() ~> HashRate) -> Int -> [Double] -> MC [Double] <- valid, but GHC bitches.
        sim _  0 hrs = return hrs
        sim w' k hrs = do
            (hr, w'') <- stepWire w' 1 ()
            let hrs' =
                case hr of
                    Left _    -> hrs'
                    Right hr' -> hr':hrs
            sim w'' (k-1) hrs

        -- simulate the wire `k' times, returning summaries of the simulations
        -- at each time step.
        runTrial :: Int -> RNG -> V.Vector Summary -> V.Vector Summary
        runTrial 0 _ s = s
        runTrial k rng s =
            let (vals, rng') = runMC (sim w steps []) rng
                vvals = V.reverse $ V.fromList vals
             in runTrial (k-1) rng' $!! V.zipWith update s vvals

     in runTrial trials (mt19937 0) (V.replicate steps (summary []))

-- | The quick and dirty form of summarize'.
summarize :: MC Double -> IO ()
summarize mc = summarize' mc "Quick" 1000000

-- | Prints out a nice summary of a random variable.
summarize' :: MC Double -- ^ The random variable to summarize.
           -> String    -- ^ The name of the variable.
           -> Int       -- ^ The number of data points requested.
           -> IO ()
summarize' mc name tries = summarizeR (mt19937 0) mc name tries >> pure ()

summarizeR :: RNG -> MC Double -> String -> Int -> IO RNG
summarizeR rng mc name tries = do
    let (rng', trials) = runTrials tries rng
        sumry = summary trials

        samples = sampleSize sumry
        min     = sampleMin sumry
        max     = sampleMax sumry
        mean    = sampleMean sumry
        se      = sampleSE sumry
        var     = sampleVar sumry
        sd      = sampleSD sumry
        ci80    = sampleCI 0.80 sumry
        ci95    = sampleCI 0.95 sumry
        ci999   = sampleCI 0.999 sumry
    
    printf "Summary - %s\n" name
    printf "-----------------------------\n"
    printf "Samples:            %d\n" samples
    printf "Min:                %.4g\n" min
    printf "Max:                %.4g\n" max
    printf "Mean:               %.4g\n" mean
    printf "Standard Error:     %.4g\n" se
    printf "Variance:           %.4g\n" var
    printf "Standard Deviation: %.4g\n" sd
    printCI "80% CI" ci80
    printCI "95% CI" ci95
    printCI "99.9% CI" ci999

    return rng'

    where
        runTrials :: Int -> RNG -> (RNG, [Double])
        runTrials 0 rng = (rng, [])
        runTrials k rng = let (t, rng') = runMC mc rng
                              (rng'', ts) = runTrials (k-1) rng'
                           in (rng'', t:ts)

        printCI :: String -> (Double, Double) -> IO ()
        printCI name (low, high) = do
            printf "%s:\n" name
            printf "  Low: %.4g\n" low
            printf "  High: %.4g\n" high
