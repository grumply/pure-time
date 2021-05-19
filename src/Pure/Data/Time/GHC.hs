{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Pure.Data.Time.GHC where

import Pure.Data.Txt
import Pure.Data.JSON

import Data.Ratio
import GHC.Generics

import Data.Hashable

import Data.Time.Clock.POSIX

-- should be able to correctly represent all whole microseconds up to 285 years
newtype Micros = Micros { getMicros :: Double }
  deriving (Show,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,Generic,ToJSON,FromJSON)

instance ToTxt Micros where
  toTxt (Micros us) = toTxt us

instance Hashable Micros where
  hashWithSalt salt (Micros us) = hashWithSalt salt us

micros :: IO Micros
micros = Micros <$> timeInMicros

-- should be able to correctly represent all whole milliseconds up to 285616 years
newtype Millis = Millis { getMillis :: Double }
  deriving (Show,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,Generic,ToJSON,FromJSON)

instance ToTxt Millis where
  toTxt (Millis ms) = toTxt ms

instance Hashable Millis where
  hashWithSalt salt (Millis ms) = hashWithSalt salt ms

millis :: IO Millis
millis = Millis <$> timeInMillis

timeInMicros :: IO Double
timeInMicros = ((* 1000000) . realToFrac) <$> getPOSIXTime

timeInMillis :: IO Double
timeInMillis = ((* 1000) . realToFrac) <$> getPOSIXTime
