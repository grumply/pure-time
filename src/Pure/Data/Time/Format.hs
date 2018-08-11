module Pure.Data.Time.Format (FormatDiffTime(..),formatDiffTimeWith) where

import Data.Time.Clock (DiffTime(..),NominalDiffTime(..))
import Data.Time.Format (TimeLocale)

import Data.Char
import Data.Fixed
import Data.Maybe

-- Since we can't use time-1.9 yet, we'll just pull in what we need manually.

type FormatNumericPadding = Maybe Char

data FormatOptions = MkFormatOptions {
    foLocale :: TimeLocale,
    foPadding :: Maybe FormatNumericPadding,
    foWidth :: Maybe Int
}

data PadOption = Pad Int Char | NoPad

class (Num t,Ord t,Show t) => ShowPadded t where
    showPaddedNum :: PadOption -> t -> String

instance ShowPadded Integer where
    showPaddedNum NoPad i = show i
    showPaddedNum pad i | i < 0 = '-':(showPaddedNum pad (negate i))
    showPaddedNum pad i = showPadded pad $ show i

instance ShowPadded Int where
    showPaddedNum NoPad i = show i
    showPaddedNum _pad i | i == minBound = show i
    showPaddedNum pad i | i < 0 = '-':(showPaddedNum pad (negate i))
    showPaddedNum pad i = showPadded pad $ show i

showPadded :: PadOption -> String -> String
showPadded NoPad s = s
showPadded (Pad i c) s = replicate (i - length s) c ++ s

showPaddedFixed :: HasResolution a => PadOption -> PadOption -> Fixed a -> String
showPaddedFixed padn padf x | x < 0 = '-' : showPaddedFixed padn padf (negate x)
showPaddedFixed padn padf x = let
    ns = showPaddedNum padn $ (floor x :: Integer)
    fs = showPaddedFixedFraction padf x
    ds = if null fs then "" else "."
    in ns ++ ds ++ fs

showPaddedFixedFraction :: HasResolution a => PadOption -> Fixed a -> String
showPaddedFixedFraction pado x = let
    digits = dropWhile (=='.') $ dropWhile (/='.') $ showFixed True x
    n = length digits
    in case pado of
        NoPad -> digits
        Pad i c -> if i < n
            then take i digits
            else digits ++ replicate (i - n) c

getPadOption :: Bool -> Bool -> Int -> Char -> Maybe FormatNumericPadding -> Maybe Int -> PadOption
getPadOption trunc fdef idef cdef mnpad mi = let
    c = case mnpad of
        Just (Just c') -> c'
        Just Nothing -> ' '
        _ -> cdef
    i = case mi of
        Just i' -> case mnpad of
            Just Nothing -> i'
            _ -> if trunc then i' else max i' idef
        Nothing -> idef
    f = case mi of
        Just _ -> True
        Nothing -> case mnpad of
            Nothing -> fdef
            Just Nothing -> False
            Just (Just _) -> True
    in if f then Pad i c else NoPad

formatNumber :: (ShowPadded i) => Bool -> Int -> Char -> (t -> i) -> (FormatOptions -> t -> String)
formatNumber fdef idef cdef ff = formatGeneral False fdef idef cdef $ \_ pado -> showPaddedNum pado . ff

formatNumberStd :: Int -> (t -> Integer) -> (FormatOptions -> t -> String)
formatNumberStd n = formatNumber False n '0'

formatGeneral :: Bool -> Bool -> Int -> Char -> (TimeLocale -> PadOption -> t -> String) -> (FormatOptions -> t -> String)
formatGeneral trunc fdef idef cdef ff fo = ff (foLocale fo) $ getPadOption trunc fdef idef cdef (foPadding fo) (foWidth fo)

formatString :: (TimeLocale -> t -> String) -> (FormatOptions -> t -> String)
formatString ff = formatGeneral False False 1 ' ' $ \locale pado -> showPadded pado . ff locale

quotBy :: (Real a,Integral b) => a -> a -> b
quotBy d n = truncate ((toRational n) / (toRational d))

remBy :: Real a => a -> a -> a
remBy d n = n - (fromInteger f) * d where
    f = quotBy d n

quotRemBy :: (Real a,Integral b) => a -> a -> (b,a)
quotRemBy d n = let
    f = quotBy d n
    in (f,n - (fromIntegral f) * d)

class FormatDiffTime t where
    formatDiffCharacter :: Bool -> Char -> Maybe (FormatOptions -> t -> String)

instance FormatDiffTime NominalDiffTime where
    formatDiffCharacter _ 'w' = Just $ formatNumberStd 1 $ quotBy $ 7 * 86400
    formatDiffCharacter _ 'd' = Just $ formatNumberStd 1 $ quotBy 86400
    formatDiffCharacter _ 'D' = Just $ formatNumberStd 1 $ remBy 7 . quotBy 86400
    formatDiffCharacter _ 'h' = Just $ formatNumberStd 1 $ quotBy 3600
    formatDiffCharacter _ 'H' = Just $ formatNumberStd 2 $ remBy 24 . quotBy 3600
    formatDiffCharacter _ 'm' = Just $ formatNumberStd 1 $ quotBy 60
    formatDiffCharacter _ 'M' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 60
    formatDiffCharacter False 's' = Just $ formatNumberStd 1 $ quotBy 1
    formatDiffCharacter True 's' = Just $ formatGeneral False False 12 '0' $ \_ padf t -> showPaddedFixed NoPad padf (realToFrac t :: Pico)
    formatDiffCharacter False 'S' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 1
    formatDiffCharacter True 'S' = Just $ formatGeneral False False 12 '0' $ \_ padf t -> let
        padn = case padf of
            NoPad -> NoPad
            Pad _ c -> Pad 2 c
        in showPaddedFixed padn padf (realToFrac $ remBy 60 t :: Pico)
    formatDiffCharacter _ _   = Nothing

instance FormatDiffTime DiffTime where
    formatDiffCharacter _ 'w' = Just $ formatNumberStd 1 $ quotBy $ 7 * 86400
    formatDiffCharacter _ 'd' = Just $ formatNumberStd 1 $ quotBy 86400
    formatDiffCharacter _ 'D' = Just $ formatNumberStd 1 $ remBy 7 . quotBy 86400
    formatDiffCharacter _ 'h' = Just $ formatNumberStd 1 $ quotBy 3600
    formatDiffCharacter _ 'H' = Just $ formatNumberStd 2 $ remBy 24 . quotBy 3600
    formatDiffCharacter _ 'm' = Just $ formatNumberStd 1 $ quotBy 60
    formatDiffCharacter _ 'M' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 60
    formatDiffCharacter False 's' = Just $ formatNumberStd 1 $ quotBy 1
    formatDiffCharacter True 's' = Just $ formatGeneral False False 12 '0' $ \_ padf t -> showPaddedFixed NoPad padf (realToFrac t :: Pico)
    formatDiffCharacter False 'S' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 1
    formatDiffCharacter True 'S' = Just $ formatGeneral False False 12 '0' $ \_ padf t -> let
        padn = case padf of
            NoPad -> NoPad
            Pad _ c -> Pad 2 c
        in showPaddedFixed padn padf (realToFrac $ remBy 60 t :: Pico)
    formatDiffCharacter _ _   = Nothing

formatDiffTimeWith :: (FormatDiffTime t) => TimeLocale -> String -> t -> String
formatDiffTimeWith _ [] _ = ""
formatDiffTimeWith locale ('%':cs) t = case formatDiffTime1 locale cs t of
    Just result -> result
    Nothing -> '%':(formatDiffTimeWith locale cs t)
formatDiffTimeWith locale (c:cs) t = c:(formatDiffTimeWith locale cs t)

formatDiffTime1 :: (FormatDiffTime t) => TimeLocale -> String -> t -> Maybe String
formatDiffTime1 locale ('_':cs) t = formatDiffTime2 locale id (Just (Just ' ')) cs t
formatDiffTime1 locale ('-':cs) t = formatDiffTime2 locale id (Just Nothing) cs t
formatDiffTime1 locale ('0':cs) t = formatDiffTime2 locale id (Just (Just '0')) cs t
formatDiffTime1 locale ('^':cs) t = formatDiffTime2 locale (fmap toUpper) Nothing cs t
formatDiffTime1 locale ('#':cs) t = formatDiffTime2 locale (fmap toLower) Nothing cs t
formatDiffTime1 locale cs t = formatDiffTime2 locale id Nothing cs t

getDigit :: Char -> Maybe Int
getDigit c | c < '0' = Nothing
getDigit c | c > '9' = Nothing
getDigit c = Just $ (ord c) - (ord '0')

pullNumber :: Maybe Int -> String -> (Maybe Int,String)
pullNumber mx [] = (mx,[])
pullNumber mx s@(c:cs) = case getDigit c of
    Just i -> pullNumber (Just $ (fromMaybe 0 mx)*10+i) cs
    Nothing -> (mx,s)

formatDiffTime2 :: (FormatDiffTime t) => TimeLocale -> (String -> String) -> Maybe FormatNumericPadding -> String -> t -> Maybe String
formatDiffTime2 locale recase mpad cs t = let
    (mwidth,rest) = pullNumber Nothing cs
    in formatDiffTime3 locale recase mpad mwidth rest t

formatDiffTime3 :: (FormatDiffTime t) => TimeLocale -> (String -> String) -> Maybe FormatNumericPadding -> Maybe Int -> String -> t -> Maybe String
formatDiffTime3 locale recase mpad mwidth ('E':cs) = formatDiffTime4 True recase (MkFormatOptions locale mpad mwidth) cs
formatDiffTime3 locale recase mpad mwidth cs = formatDiffTime4 False recase (MkFormatOptions locale mpad mwidth) cs

formatDiffTime4 :: (FormatDiffTime t) => Bool -> (String -> String) -> FormatOptions -> String -> t -> Maybe String
formatDiffTime4 alt recase fo (c:cs) t = Just $ (recase (formatChar alt c fo t)) ++ (formatDiffTimeWith (foLocale fo) cs t)
formatDiffTime4 _alt _recase _fo [] _t = Nothing

formatChar :: (FormatDiffTime t) => Bool -> Char -> FormatOptions -> t -> String
formatChar _ '%' = formatString $ \_ _ -> "%"
formatChar _ 't' = formatString $ \_ _ -> "\t"
formatChar _ 'n' = formatString $ \_ _ -> "\n"
formatChar alt c = case formatDiffCharacter alt c of
    Just f -> f
    _ -> \_ _ -> ""