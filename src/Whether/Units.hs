{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Whether.Units where

import GHC.Generics
import Toml
import Toml.Schema
import qualified Data.Text as S ( Text )

data UnitSystem = Standard | Metric | Imperial
  deriving (Eq, Read, Show, Generic)

-- Cache of the unit system used to query most recent data
newtype UnitSystemCache = UnitSystemCache { cachedUnitSystem :: UnitSystem }
  deriving (Eq, Read, Show, Generic)

data TimeNotation = TwelveHour | TwentyFourHour
  deriving (Eq, Read, Show, Generic)

-- | A record containing the datetime mode of all @(Forecast) records. 
data DTStyle = 
  DTStyle
  { dayStyle     :: DayStyle
  , currentStyle :: CurrentStyle
  , timeNotation :: TimeNotation
  }

-- | Represents the style used to display the day.
data DayStyle = DayAbbr | DayMonth | MonthDay
  deriving (Eq, Read, Show, Generic)

-- | Represents the style used to display the current date and/or time,
-- parameterized by @(TimeNotation).
data CurrentStyle = HourMinute
                  | DayNameHourMinute
                  | MonthDayHourMinute
                  | YearMonthDayHourMinute
  deriving (Eq, Read, Show, Generic)

data CardinalDirection = NorthWest | North | NorthEast
                       | West              | East
                       | SouthWest | South | SouthEast

data Precipitation = Inch Double | Millimetre Double

data Speed = MilesPerHour Double | MetresPerSecond Double

data Temperature = Kelvin Double | Celsius Double | Fahrenheit Double

data PressureLevel = HighPressure | NormalPressure | LowPressure

newtype Pressure = Pressure Double

newtype RelativeHumidity = RelativeHumidity Integer

newtype UVI = UVI Double

liftT :: (Double -> Double) -> Temperature -> Temperature
liftT f (Fahrenheit t) = Fahrenheit (f t)
liftT f (Celsius t)    = Celsius (f t)
liftT f (Kelvin t)     = Kelvin (f t)

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
toHeatIndex :: Temperature -> RelativeHumidity -> Temperature
toHeatIndex te (RelativeHumidity rhI) = case te of
  Fahrenheit _  ->             heatIndexF te
  Celsius    _  -> toCelsius $ heatIndexF te
  Kelvin     _  -> toKelvin  $ heatIndexF te
  where
    heatIndexF (Fahrenheit t) = heatIndex t
    heatIndexF t              = heatIndexF $ toFahrenheit t
    heatIndex t
      | adjHI < 80 = Fahrenheit $ 0.5 * (t + 61 + ((t - 68) * 1.2) + (rh * 0.094))
      | otherwise  = Fahrenheit adjHI
        where
          rh = fromIntegral rhI
          hi = -42.379 + 2.04901523*t + 10.14333127*rh - 0.22475541*t*rh - 0.00683783*t*t - 0.05481717*rh*rh + 0.00122874*t*t*rh + 0.00085282*t*rh*rh - 0.00000199*t*t*rh*rh
          adjHI
              | rh < 0.13 && t > 80 && t < 112 = hi - ((13 - rh) / 4) * sqrt ((17 - abs (t - 95)) / 17)
              | rh > 0.85 && t > 80 && t < 87 = hi + ((rh - 85) / 10) * ((87 - t) / 5)
              | otherwise = hi
