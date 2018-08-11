{-# LANGUAGE ViewPatterns, PatternSynonyms, DeriveGeneric, ScopedTypeVariables, CPP, GeneralizedNewtypeDeriving, InstanceSigs #-}
module Pure.Data.Time (module Pure.Data.Time, module Export) where

import Data.Coerce
import GHC.Generics

import Pure.Data.JSON

import Pure.Data.Time.Internal as Export

import Data.Time.Format (FormatTime(..),ParseTime(..))
import Data.Time.LocalTime (utc,utcToZonedTime)

import Unsafe.Coerce -- for NominalDiffTime <-> DiffTime

newtype Time = Time { getTime :: Millis }
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

pattern Second :: Time
pattern Second = Time 1000

pattern Minute :: Time
pattern Minute = Time 60000

pattern Hour :: Time
pattern Hour = Time 3600000

pattern Day :: Time
pattern Day = Time 86400000

pattern Week :: Time
pattern Week = Time 604800000

pattern Month :: Time
pattern Month = Time 2628000000

pattern Year :: Time
pattern Year = Time 31536000000

time :: IO Time
time = coerce <$> millis

nominalDiffTime :: Time -> Time -> NominalDiffTime
nominalDiffTime a b = diffMillis (fromTime a) (fromTime b)

diffTime :: Time -> Time -> DiffTime
diffTime a b = unsafeCoerce (nominalDiffTime a b)

seconds :: Int -> Time
seconds s = Second * (fromIntegral s)

minutes :: Int -> Time
minutes m = Minute * (fromIntegral m)

hours :: Int -> Time
hours h = Hour * (fromIntegral h)

days :: Int -> Time
days d = Day * (fromIntegral d)

weeks :: Int -> Time
weeks w = Week * (fromIntegral w)

months :: Int -> Time
months m = Month * (fromIntegral m)

years :: Int -> Time
years y = Year * (fromIntegral y)

-- Careful with this; it's reasonably safe (to a millisecond) when
-- used on proper Time values.
quotRemTime :: Time -> Time -> (Int,Time)
quotRemTime n d = 
  let (q,r) = quotRem (round n) (round d)
  in (q,fromIntegral r)

pattern Seconds :: Int -> Time
pattern Seconds ss <- ((`div` (round Second)) . round -> ss) where
    Seconds ss = seconds ss

pattern Minutes :: Int -> Time -> Time
pattern Minutes ms rest <- ((`quotRemTime` Minute) -> (ms,rest)) where
    Minutes ms rest = minutes ms + rest 

pattern Hours :: Int -> Time -> Time
pattern Hours hs rest <- ((`quotRemTime` Hour) -> (hs,rest)) where
    Hours hs rest = hours hs + rest

pattern Days :: Int -> Time -> Time
pattern Days ds rest <- ((`quotRemTime` Day) -> (ds,rest)) where
    Days ds rest = days ds + rest

pattern Weeks :: Int -> Time -> Time
pattern Weeks ws rest <- ((`quotRemTime` Week) -> (ws,rest)) where
    Weeks ws rest = weeks ws + rest

pattern Months :: Int -> Time -> Time
pattern Months ms rest <- ((`quotRemTime` Month) -> (ms,rest)) where
    Months ms rest = months ms + rest

pattern Years :: Int -> Time -> Time
pattern Years ys rest <- ((`quotRemTime` Year) -> (ys,rest)) where
    Years ys rest = years ys + rest