{-# LANGUAGE OverloadedStrings #-}

module Conversions where

import Types
import Data.Ix            ( inRange )
import Data.ByteString    ( ByteString )
import Data.Text          ( Text, pack )
import Data.Text.Encoding ( encodeUtf8 )

import qualified Types as C ( Current(dt, sunrise, sunset) )

-- (-...) :: Temperature -> Temperature -> Temperature
-- Fahrenheit t      -... Fahrenheit t' = Fahrenheit (t - t')
-- tu@(Fahrenheit _) -... tu'           = tu -... (toFahrenheit tu')
-- Celsius t         -... Celsius t'    = Celsius (t - t')
-- tu@(Celsius _)    -... tu'           = tu -... (toCelsius tu')
-- Kelvin t          -... Kelvin t'     = Kelvin (t - t')
-- tu@(Kelvin _)     -... tu'           = tu -... (toKelvin tu')

liftT :: (Double -> Double) -> Temperature -> Temperature
liftT f (Fahrenheit t) = Fahrenheit (f t)
liftT f (Celsius t)    = Celsius (f t)
liftT f (Kelvin t)     = Kelvin (f t)

numT :: Temperature -> Double
numT (Fahrenheit t) = t
numT (Celsius t)    = t
numT (Kelvin t)     = t

liftT2 :: (Double -> Double -> Double) -> Temperature -> Temperature -> Temperature
liftT2 f (Fahrenheit t)    (Fahrenheit t') = Fahrenheit (f t t')
liftT2 f ut@(Fahrenheit _) ut'             = liftT2 f ut (toFahrenheit ut')
liftT2 f (Celsius t)       (Celsius t')    = Celsius    (f t t')
liftT2 f ut@(Celsius _)    ut'             = liftT2 f ut (toCelsius ut')
liftT2 f (Kelvin t)        (Kelvin t')     = Kelvin (f t t')
liftT2 f ut@(Kelvin _)     ut'             = liftT2 f ut (toKelvin ut')

toFahrenheit :: Temperature -> Temperature
toFahrenheit (Fahrenheit t) = Fahrenheit t
toFahrenheit (Celsius t)    = Fahrenheit $ t * 1.8 + 32
toFahrenheit (Kelvin t)     = Fahrenheit $ (t - 273.15) * 1.8 + 32

toCelsius :: Temperature -> Temperature
toCelsius (Fahrenheit t) = Celsius $ (t - 32) * (5 / 9 :: Double)
toCelsius (Celsius t)    = Celsius t
toCelsius (Kelvin t)     = Celsius $ t - 273.15

toKelvin :: Temperature -> Temperature
toKelvin (Fahrenheit t) = Kelvin $ (t - 32) * (5 / 9 :: Double) + 273.15
toKelvin (Celsius t)    = Kelvin $ t + 273.15
toKelvin (Kelvin t)     = Kelvin t
-- roundT :: Temperature -> Temperature
-- roundT = 
-- roundT (Fahrenheit t) = Fahrenheit (round1 t)
-- roundT (Celsius t)    = Celsius    (round1 t)
-- roundT (Kelvin t)     = Kelvin     (round1 t)

round1 :: Double -> Double
round1 x = fromIntegral (floor (x + 0.5) :: Integer) :: Double

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

toPrecipitation :: UnitSystem -> Double -> Precipitation
toPrecipitation Imperial p = Inch p
toPrecipitation _ p = Millimetre p

toTemperature :: UnitSystem -> Double -> Temperature
toTemperature Imperial t = Fahrenheit t
toTemperature Metric t   = Celsius t
toTemperature Standard t = Kelvin t

toSpeed :: UnitSystem -> Double -> Speed
toSpeed Imperial s = MilesPerHour s
toSpeed _ s = MetresPerSecond s

toPressureLevel :: Integer -> PressureLevel
toPressureLevel p
    | p > 1014  = HighPressure
    | p < 1012  = LowPressure
    | otherwise = NormalPressure

-- Formula: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
toHeatIndex :: Temperature -> Integer -> Temperature
toHeatIndex temperature rhI = out
  where
    out = case temperature of
        Fahrenheit _  -> finalHI
        Celsius    _  -> toCelsius finalHI
        Kelvin     _  -> toKelvin finalHI
    finalHI = heatIndex (toFahrenheit temperature)
    heatIndex (Fahrenheit t)
        | adjHI < 80 = Fahrenheit $ 0.5 * (t + 61 + ((t - 68) * 1.2) + (rh * 0.094))
        | otherwise  = Fahrenheit adjHI
          where
            rh = fromIntegral rhI
            hi = -42.379 + 2.04901523*t + 10.14333127*rh - 0.22475541*t*rh - 0.00683783*t*t - 0.05481717*rh*rh + 0.00122874*t*t*rh + 0.00085282*t*rh*rh - 0.00000199*t*t*rh*rh
            adjHI
                | rh < 0.13 && t > 80 && t < 112 = hi - ((13 - rh) / 4) * sqrt ((17 - (abs (t - 95))) / 17)
                | rh > 0.85 && t > 80 && t < 87 = hi + ((rh - 85) / 10) * ((87 - t) / 5)
                | otherwise = hi
    heatIndex _ = error "heatIndex is not implemented for Celsius or Kelvin"


