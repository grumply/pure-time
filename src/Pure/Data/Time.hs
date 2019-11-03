{-# LANGUAGE ViewPatterns, PatternSynonyms, DeriveGeneric, ScopedTypeVariables, CPP, GeneralizedNewtypeDeriving, InstanceSigs, RecordWildCards, OverloadedStrings #-}
module Pure.Data.Time (module Pure.Data.Time, module Export) where

import Control.Concurrent (threadDelay)
import Data.Coerce
import GHC.Generics

import Pure.Data.Default

import Pure.Data.JSON

import Pure.Data.Time.Internal as Export

import Data.Time.Format (FormatTime(..),ParseTime(..))
import Data.Time.LocalTime (utc,utcToZonedTime)

import Unsafe.Coerce -- for NominalDiffTime <-> DiffTime

-- Time and TimeDiff are designed to be stupid easy for the vast majority of use cases
--
-- Bidirectional patterns:
--
-- > let Hours h (Minutes m (Seconds s)) = Seconds 128000
--
-- > let Hours h _ = Seconds 128000
--
-- > let Seconds s = Hours 35 (Minutes 3 0)
--
-- > formatTime "%h" (Hours 3 0)
--
-- > formatTime "%w weeks and %d days" (diffTime t1 t2)
--
-- > let TimeDiff {..} = timeDiff t1 t2
-- >     ft = if Time td > Week then "%w" else "%h"
-- > in ft TimeDiff {..}

newtype Time = Time_ { getTime :: Millis }
    deriving (Show,Eq,Ord,Generic,Num,Real,Fractional,Floating,RealFrac,ToJSON,FromJSON)

instance FormatTime Time where
#if MIN_VERSION_time(1,8,0)
  formatCharacter c = fmap (\f l p w t -> f l p w (utcToZonedTime utc (utcTimeFromMillis (getTime t)))) (formatCharacter c)
#else
  formatCharacter c = fmap (\f l p t -> f l p (utcToZonedTime utc (utcTimeFromMillis (getTime t)))) (formatCharacter c)
#endif

instance ParseTime Time where
  buildTime tl s = fmap (coerce . utcTimeToMillis) (buildTime tl s)

class IsTime t where
    toTime :: t -> Time
    fromTime :: Time -> t

instance IsTime Time where
    toTime   = id
    fromTime = id

instance IsTime Millis where
    toTime   = coerce
    fromTime = coerce

instance IsTime Micros where
    toTime   = coerce . (/ 1000)
    fromTime = coerce . (* 1000)

instance IsTime DiffTime where
    toTime   = coerce . diffTimeToMillis
    fromTime = millisToDiffTime . coerce

instance IsTime NominalDiffTime where
    toTime   = toTime . (unsafeCoerce :: NominalDiffTime -> DiffTime)
    fromTime = (unsafeCoerce :: DiffTime -> NominalDiffTime) . fromTime

pattern Time :: IsTime t => t -> Time
pattern Time t <- (fromTime -> t) where
    Time t = toTime t

pattern Nanosecond :: Time
pattern Nanosecond = 1e-6

pattern Microsecond :: Time
pattern Microsecond = 0.001

pattern Millisecond :: Time
pattern Millisecond = 1

pattern Second :: Time
pattern Second = 1000

pattern Minute :: Time
pattern Minute = 60000

pattern Hour :: Time
pattern Hour = 3600000

pattern Day :: Time
pattern Day = 86400000

pattern Week :: Time
pattern Week = 604800000

-- 30 days + 10 hours + 29 minutes + 6 seconds
-- the average month in the 400 year gregorian cycle
pattern Month :: Time
pattern Month = 2629746000

-- 365.2422 days
-- the average year in the 400 year gregorian cycle
-- if you want a 365-day year, use (Day * 365).
pattern Year :: Time
pattern Year = 31556926080

time :: IO Time
time = coerce <$> millis

nominalDiffTime :: Time -> Time -> NominalDiffTime
nominalDiffTime a b = diffMillis (fromTime a) (fromTime b)

diffTime :: Time -> Time -> DiffTime
diffTime a b = unsafeCoerce (nominalDiffTime a b)

pattern Nanoseconds :: Int -> Time
pattern Nanoseconds ns <- (round . (/ Nanosecond) -> ns) where
    Nanoseconds ns = Nanosecond * (fromIntegral ns)

pattern Microseconds :: Int -> Time -> Time
pattern Microseconds us rest <- (fmap (* Microsecond) . properFraction . (/ Microsecond) -> (us,rest)) where
    Microseconds us rest = Microsecond * (fromIntegral us) + rest

pattern Milliseconds :: Int -> Time -> Time
pattern Milliseconds ms rest <- (fmap (* Millisecond) . properFraction . (/ Millisecond) -> (ms,rest)) where
    Milliseconds ms rest = Millisecond * (fromIntegral ms) + rest

pattern Seconds :: Int -> Time -> Time
pattern Seconds ss rest <- (fmap (* Second) . properFraction . (/ Second) -> (ss,rest)) where
    Seconds ss rest = Second * (fromIntegral ss) + rest

pattern Minutes :: Int -> Time -> Time
pattern Minutes ms rest <- (fmap (* Minute) . properFraction . (/ Minute) -> (ms,rest)) where
    Minutes ms rest = Minute * (fromIntegral ms) + rest

pattern Hours :: Int -> Time -> Time
pattern Hours hs rest <- (fmap (* Hour) . properFraction . (/ Hour) -> (hs,rest)) where
    Hours hs rest = Hour * (fromIntegral hs) + rest

pattern Days :: Int -> Time -> Time
pattern Days ds rest <- (fmap (* Day) . properFraction . (/ Day) -> (ds,rest)) where
    Days ds rest = Day * (fromIntegral ds) + rest

pattern Weeks :: Int -> Time -> Time
pattern Weeks ws rest <- (fmap (* Week) . properFraction . (/ Week) -> (ws,rest)) where
    Weeks ws rest = Week * (fromIntegral ws) + rest

pattern Months :: Int -> Time -> Time
pattern Months ms rest <- (fmap (* Month) . properFraction . (/ Month) -> (ms,rest)) where
    Months ms rest = Month * (fromIntegral ms) + rest

pattern Years :: Int -> Time -> Time
pattern Years ys rest <- (fmap (* Year) . properFraction . (/ Year) -> (ys,rest)) where
    Years ys rest = Year * (fromIntegral ys) + rest

data TimeDiff = TimeDiff
    { milliseconds :: Double
    , seconds :: Double
    , minutes :: Double
    , hours   :: Double
    , days    :: Double
    , weeks   :: Double
    , months  :: Double
    , years   :: Double
    } deriving (Show,Generic)

instance Default TimeDiff where
    def = TimeDiff 0 0 0 0 0 0 0 0

instance Eq TimeDiff where
    (==) t1 t2 = milliseconds t1 == milliseconds t2

instance Ord TimeDiff where
    compare t1 t2 = compare (milliseconds t1) (milliseconds t2)

instance IsTime TimeDiff where
    toTime = coerce . (1000 *) . seconds
    fromTime = flip timeDiff (def :: TimeDiff)

instance FormatTime TimeDiff where
#if MIN_VERSION_time(1,8,0)
  formatCharacter c = fmap (\f l p w t -> f l p w (utcToZonedTime utc (utcTimeFromMillis (getTime (toTime t))))) (formatCharacter c)
#else
  formatCharacter c = fmap (\f l p t -> f l p (utcToZonedTime utc (utcTimeFromMillis (getTime (toTime t))))) (formatCharacter c)
#endif

instance ParseTime TimeDiff where
  buildTime tl s = fmap (fromTime . toTime . utcTimeToMillis) (buildTime tl s)

timeDiff :: (IsTime a, IsTime b) => a -> b -> TimeDiff
timeDiff (getTime . toTime -> Millis now) (getTime . toTime -> Millis ago) = TimeDiff {..}
    where
        d = now - ago
        second  = 1000
        minute  = 60    * second
        hour    = 60    * minute
        day     = 24    * hour
        week    = 7     * day
        month   = 30.42 * day
        year    = 365   * day
        milliseconds = d
        seconds = d / second
        minutes = d / minute
        hours   = d / hour
        days    = d / day
        weeks   = d / week
        months  = d / month
        years   = d / year

timeDiffToDiffTime :: TimeDiff -> DiffTime
timeDiffToDiffTime = millisToDiffTime . getTime . toTime

diffTimeToTimeDiff :: DiffTime -> TimeDiff
diffTimeToTimeDiff = fromTime . coerce . diffTimeToMillis

delay :: Time -> IO ()
delay (Microseconds n _) = threadDelay n