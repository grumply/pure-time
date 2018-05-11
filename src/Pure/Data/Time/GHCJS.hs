{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, JavaScriptFFI #-}
module Pure.Data.Time.GHCJS where

import Pure.Data.Txt
import Pure.Data.JSON

import GHC.Generics

import Data.Ratio
import Data.Hashable

-- microseconds since beginning of 1970
newtype Micros = Micros { getMicros :: Double }
  deriving (Show,Eq,Ord,Num,Real,Generic,ToJSON,FromJSON)

instance ToTxt Micros where
  toTxt (Micros us) = toTxt us

instance Hashable Micros where
  hashWithSalt salt (Micros us) = hashWithSalt salt us

micros :: IO Micros
micros = Micros <$> timeInMicros

timeInMicros =
  (*1000) <$> getTime_micros_js

-- milliseconds since beginning of 1970
newtype Millis = Millis { getMillis :: Double }
  deriving (Show,Eq,Ord,Num,Real,Generic,ToJSON,FromJSON)

instance ToTxt Millis where
  toTxt (Millis ms) = toTxt ms

instance Hashable Millis where
  hashWithSalt salt (Millis ms) = hashWithSalt salt ms

millis :: IO Millis
millis = Millis <$> timeInMillis

timeInMillis =
  getTime_millis_js

foreign import javascript unsafe
  "if (performance.now !== 'undefined') { $r = performance.now() + performance.timing.navigationStart } else { $r = new Date().getTime() }" getTime_micros_js :: IO Double


foreign import javascript unsafe
  "$r = new Date().getTime();" getTime_millis_js :: IO Double
