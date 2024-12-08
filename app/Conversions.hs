{-# LANGUAGE OverloadedStrings #-}

module Conversions where

import Types
import Data.Ix            ( inRange )
import Data.ByteString    ( ByteString )
import Data.Text          ( Text, pack )
import Data.Text.Encoding ( encodeUtf8 )

import qualified Types as C ( Current(dt, sunrise, sunset) )

-- Convert the OWM moon_phase float to a defined MoonPhase type
toMoonPhase :: Double -> Maybe MoonPhase
toMoonPhase x
    | x `elem` [0, 1]          = Just NewMoon
    | (x > 0   ) && (x < 0.25) = Just WaxingCrescent
    | x == 0.25                = Just FirstQuarter
    | (x > 0.25) && (x < 0.5)  = Just WaxingGibbous
    | x == 0.50                = Just FullMoon
    | (x > 0.50) && (x < 0.75) = Just WaningGibbous
    | x == 0.75                = Just LastQuarter
    | (x > 0.75) && (x < 1.00) = Just WaningCrescent
    | otherwise                = Nothing

-- Convert the OWM weather condition code to a defined type
toWeatherCondition :: Bool -> Integer -> Maybe WeatherCondition
toWeatherCondition isDay x
    |  inRange  (200, 299) x  = Just Thunderstorm
    |  inRange  (300, 399) x
    || inRange  (502, 599) x  = Just Rain
    |  inRange  (500, 501) x  = Just RainPartial
    |  inRange  (600, 699) x  = Just Snow
    |  x == 701               = Just Mist
    |  x == 711               = Just Smoke
    |  x `elem` [721, 731,
                 751, 761]    = Just Haze
    |  x == 741               = Just Fog
    |  x == 781               = Just Tornado
    |  x == 800 && isDay      = Just ClearDay
    |  x == 800               = Just ClearNight
    |  x == 801               = Just PartlyCloudy
    |  x `elem` [802, 803]    = Just MostlyCloudy
    |  x == 804               = Just Cloudy
    |  otherwise              = Nothing

toPressureSymbol :: PressureLevel -> Text
toPressureSymbol pl = case pl of
    HighPressure   -> "🅗 "
    NormalPressure -> "⚫"
    LowPressure    -> "🅛 "

-- Convert the angle of the wind (in degrees) to a cardinal direction
toCardinalDirection :: Integer -> Maybe CardinalDirection
toCardinalDirection x 
    |  inRange(0, 22)    x 
    || inRange(338, 359) x = Just North
    |  inRange(23, 67)   x = Just NorthEast
    |  inRange(68, 112)  x = Just East
    |  inRange(113, 157) x = Just SouthEast
    |  inRange(158, 202) x = Just South
    |  inRange(203, 247) x = Just SouthWest
    |  inRange(248, 292) x = Just West
    |  inRange(293, 337) x = Just NorthWest
    |  otherwise           = Nothing

toUVIDescription :: Double -> String
toUVIDescription x 
    | 0    <= x && x < 2.5  = "Low"
    | 2.5  <= x && x < 5.5  = "Moderate"
    | 5.5  <= x && x < 7.5  = "High"
    | 7.5  <= x && x < 10.5 = "Very High"
    | 10.5 <= x             = "Extreme"
    | otherwise             = "(Out of Range)"

-- Check whether the time of day is between sunrise and sunset
isDayCurrent :: Current -> Bool
isDayCurrent c = C.dt c >= C.sunrise c && C.dt c <= C.sunset c

-- Format input values for use in a OneCall API call
formatCoord :: Double -> ByteString
formatCoord x = encodeUtf8 $ pack $ show x

formatTUnits :: UnitSystem -> ByteString
formatTUnits x = encodeUtf8 $ pack $ show x

toPrecipitation :: Config -> Double -> Precipitation
toPrecipitation config p = case (unitSystem config) of 
    Imperial -> Inch (p / 25.4) 
    _        -> Millimetre p

toPressureLevel :: Integer -> PressureLevel
toPressureLevel p
    | p > 1014  = HighPressure
    | p < 1012  = LowPressure
    | otherwise = NormalPressure

toHeatIndex :: Temperature -> Integer -> Temperature
toHeatIndex (T u baseTemp) rhPercent = T u finalHI
  where
    finalHI
        | adjHI < 80 = 0.5 * (t + 61 + ((t - 68) * 1.2) + (rh * 0.094))
        | otherwise  = adjHI
    adjHI
        | rh < 0.13 && t > 80 && t < 112 = hi + ((13 - rh) / 4) * sqrt ((17 - (abs 95)) / 17)
        | rh > 0.85 && t > 80 && t < 87 = hi + ((rh - 85) / 10) * ((87 - t) / 5)
        | otherwise = hi
    hi = -42.379 + 2.04901523*t + 10.14333127*rh - 0.22475541*t*rh - 0.00683783*t*t - 0.05481717*rh*rh + 0.00122874*t*t*rh + 0.00085282*t*rh*rh - 0.00000199*t*t*rh*rh
    rh = (fromIntegral rhPercent) * 0.01
    t = case u of
        Imperial -> t
        Metric   -> 32 + baseTemp * 1.8
        Standard -> 32 + (baseTemp - 273.15) * 1.8

