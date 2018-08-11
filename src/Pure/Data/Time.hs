{-# LANGUAGE ViewPatterns, CPP #-}
module Pure.Data.Time (module Export, module Pure.Data.Time) where

#ifdef __GHCJS__
import Pure.Data.Time.GHCJS as Export
#else
import Pure.Data.Time.GHC as Export
#endif

import Pure.Data.Time.Format

import Data.Ratio

import Pure.Data.Txt

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format (FormatTime(..),ParseTime(..))
import Data.Time.LocalTime (utc,utcToZonedTime)

import qualified Data.Time.Format as Format

instance FormatTime Millis where
#if MIN_VERSION_time(1,8,0)
  formatCharacter c = fmap (\f l p w t -> f l p w (utcToZonedTime utc (utcTimeFromMillis t))) (formatCharacter c)
#else
  formatCharacter c = fmap (\f l p t -> f l p (utcToZonedTime utc (utcTimeFromMillis t))) (formatCharacter c)
#endif

instance ParseTime Millis where
  buildTime tl s = fmap utcTimeToMillis (buildTime tl s)

instance FormatTime Micros where
#if MIN_VERSION_time(1,8,0)
  formatCharacter c = fmap (\f l p w t -> f l p w (utcToZonedTime utc (utcTimeFromMicros t))) (formatCharacter c)
#else
  formatCharacter c = fmap (\f l p t -> f l p (utcToZonedTime utc (utcTimeFromMicros t))) (formatCharacter c)
#endif

instance ParseTime Micros where
  buildTime tl s = fmap utcTimeToMicros (buildTime tl s)

formatTime :: (FromTxt txt, FormatTime t) => String -> t -> txt
formatTime s t = fromTxt $ toTxt (Format.formatTime Format.defaultTimeLocale s t)

toDate :: (FromTxt txt, FormatTime t) => t -> txt
toDate = formatTime "%Y-%m-%d"

toDateTime :: (FromTxt txt, FormatTime t) => t -> txt
#if MIN_VERSION_time(1,8,0)
toDateTime = formatTime "%Y-%m-%dT%H:%M:%S%03Q"
#else
toDateTime = formatTime "%Y-%m-%dT%H:%M:%S%Q"
#endif

toZonedDateTime :: (FromTxt txt, FormatTime t) => t -> txt
#if MIN_VERSION_time(1,8,0)
toZonedDateTime = formatTime "%Y-%m-%dT%H:%M:%S%03Q%z"
#else
toZonedDateTime = formatTime "%Y-%m-%dT%H:%M:%S%Q%z"
#endif

toPrettyTime :: (FromTxt txt, FormatTime t) => t -> txt
toPrettyTime = formatTime "%b%e, %Y"

parseTime :: (ParseTime t) => String -> Txt -> Maybe t
parseTime f i = Format.parseTimeM True Format.defaultTimeLocale f (fromTxt i)

fromDate :: ParseTime t => Txt -> Maybe t
fromDate = parseTime "%Y-%m-%d"

fromDateTime :: ParseTime t => Txt -> Maybe t
#if MIN_VERSION_time(1,8,0)
fromDateTime = parseTime "%Y-%m-%dT%H:%M:%S%03Q"
#else
fromDateTime = parseTime "%Y-%m-%dT%H:%M:%S%Q"
#endif

fromZonedDateTime :: ParseTime t => Txt -> Maybe t
#if MIN_VERSION_time(1,8,0)
fromZonedDateTime = parseTime "%Y-%m-%dT%H:%M:%S%03Q%z"
#else
fromZonedDateTime = parseTime "%Y-%m-%dT%H:%M:%S%Q%z"
#endif

fromPrettyTime :: ParseTime t => Txt -> Maybe t
fromPrettyTime = parseTime "%b%e, %Y"

posixToMillis :: POSIXTime -> Millis
posixToMillis =
    Millis
  . fromIntegral
  . (`div` 1000)
  . numerator
  . toRational
  . (* 1000000)

posixFromMillis :: Millis -> POSIXTime
posixFromMillis =
  fromRational
  . (% 1000000)
  . (* 1000)
  . round
  . getMillis

utcTimeToMillis :: UTCTime -> Millis
utcTimeToMillis =
    posixToMillis
  . utcTimeToPOSIXSeconds

utcTimeFromMillis :: Millis -> UTCTime
utcTimeFromMillis =
    posixSecondsToUTCTime
  . posixFromMillis

posixToMicros :: POSIXTime -> Micros
posixToMicros =
    Micros
  . fromIntegral
  . numerator
  . toRational
  . (* 1000000)

posixFromMicros :: Micros -> POSIXTime
posixFromMicros =
  fromRational
  . (% 1000000)
  . round
  . getMicros

utcTimeToMicros :: UTCTime -> Micros
utcTimeToMicros =
    posixToMicros
  . utcTimeToPOSIXSeconds

utcTimeFromMicros :: Micros -> UTCTime
utcTimeFromMicros =
    posixSecondsToUTCTime
  . posixFromMicros

millisToDiffTime :: Millis -> DiffTime
millisToDiffTime = picosecondsToDiffTime . round . (* 1e9) . getMillis

diffTimeToMillis :: DiffTime -> Millis
diffTimeToMillis = Millis . fromInteger . (`div` 1000000000) . diffTimeToPicoseconds

microsToDiffTime :: Micros -> DiffTime
microsToDiffTime = picosecondsToDiffTime . round . (* 1e6) . getMicros

diffTimeToMicros :: DiffTime -> Micros
diffTimeToMicros = Micros . fromInteger . (`div` 1000000) . diffTimeToPicoseconds

diffMillis :: Millis -> Millis -> NominalDiffTime
diffMillis (utcTimeFromMillis -> a) (utcTimeFromMillis -> b) = diffUTCTime a b

diffMicros :: Micros -> Micros -> NominalDiffTime
diffMicros (utcTimeFromMicros -> a) (utcTimeFromMicros -> b) = diffUTCTime a b

formatDiffTime :: (FromTxt txt, FormatDiffTime t) => String -> t -> txt
formatDiffTime fs = fromTxt . toTxt . formatDiffTimeWith Format.defaultTimeLocale fs