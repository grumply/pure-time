{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Pure.Data.Time.GHC where

import Pure.Data.Txt
import Pure.Data.JSON

import GHC.Generics

import Data.Ratio
import Data.Hashable

import Data.Time.Clock
import Data.Time.Clock.POSIX

-- microseconds since beginning of 1970
newtype Micros = Micros { getMicros :: Double }
  deriving (Show,Eq,Ord,Num,Real,Generic,ToJSON,FromJSON)

instance ToTxt Micros where
  toTxt (Micros us) = toTxt us

instance Hashable Micros where
  hashWithSalt salt (Micros us) = hashWithSalt salt us

micros :: IO Micros
micros = Micros <$> timeInMicros

-- milliseconds since beginning of 1970
newtype Millis = Millis { getMillis :: Double }
  deriving (Show,Eq,Ord,Num,Real,Generic,ToJSON,FromJSON)

instance ToTxt Millis where
  toTxt (Millis ms) = toTxt ms

instance Hashable Millis where
  hashWithSalt salt (Millis ms) = hashWithSalt salt ms

millis :: IO Millis
millis = Millis <$> timeInMillis

timeInMicros :: IO Double
timeInMicros = posixToMicros <$> getPOSIXTime

timeInMillis :: IO Double
timeInMillis = posixToMillis <$> getPOSIXTime

posixToMillis :: POSIXTime -> Double
posixToMillis =
    fromIntegral
  . (`div` 1000)
  . numerator
  . toRational
  . (* 1000000)

posixFromMillis :: Double -> POSIXTime
posixFromMillis =
  fromRational
  . (% 1000000)
  . (* 1000)
  . round

utcTimeToMillis :: UTCTime -> Double
utcTimeToMillis =
    posixToMillis
  . utcTimeToPOSIXSeconds

utcTimeFromMillis :: Double -> UTCTime
utcTimeFromMillis =
    posixSecondsToUTCTime
  . posixFromMillis

posixToMicros :: POSIXTime -> Double
posixToMicros =
    fromIntegral
  . numerator
  . toRational
  . (* 1000000)

posixFromMicros :: Double -> POSIXTime
posixFromMicros =
  fromRational
  . (% 1000000)
  . round

utcTimeToMicros :: UTCTime -> Double
utcTimeToMicros =
    posixToMicros
  . utcTimeToPOSIXSeconds

utcTimeFromMicros :: Double -> UTCTime
utcTimeFromMicros =
    posixSecondsToUTCTime
  . posixFromMicros
