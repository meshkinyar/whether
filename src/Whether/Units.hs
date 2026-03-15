{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Whether.Units where

import GHC.Generics
import Toml
import Toml.Schema
import qualified Data.Text as S ( Text )

data UnitSystem = Standard | Metric | Imperial
  deriving (Eq, Read, Show, Generic)

-- | Cache of the unit system used to query most recent data
newtype UnitSystemCache = UnitSystemCache { cachedUnitSystem :: UnitSystem }
  deriving (Eq, Read, Show, Generic)

-- | Notation style for displaying timestamps.
data TimeNotation = TwelveHour | TwentyFourHour
  deriving (Eq, Read, Show, Generic)

-- | A record containing the datetime mode of all @Forecast@ records. 
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
-- parameterized by @TimeNotation@.
data CurrentStyle = HourMinute
                  | DayNameHourMinute
                  | MonthDayHourMinute
                  | YearMonthDayHourMinute
  deriving (Eq, Read, Show, Generic)

-- | Represents cardinal direction.
data CardinalDirection = NorthWest | North | NorthEast
                       | West              | East
                       | SouthWest | South | SouthEast

-- | Represents the depth of precipitation with units.
data Precipitation = Inch Double | Millimetre Double

-- | Represents a speed scalar with units.
data Speed = MilesPerHour Double | MetresPerSecond Double

-- | Represents temperature under the three main unit systems.
data Temperature = Kelvin Double | Celsius Double | Fahrenheit Double

-- | Represents whether the pressure value is considered high, low, or normal.
data PressureLevel = HighPressure | NormalPressure | LowPressure

-- | Represents pressure in hectopascals (hPa).
newtype Pressure = HectoPascal Double

-- | Represents % relative humidity.
newtype RelativeHumidity = RelativeHumidity Integer

-- | Represents the UV index value.
newtype UVI = UVI Double

-- | Lifting function for functions that take one @Temperature@ value.
liftT :: (Double -> Double) -> Temperature -> Temperature
liftT f (Fahrenheit t) = Fahrenheit (f t)
liftT f (Celsius t)    = Celsius (f t)
liftT f (Kelvin t)     = Kelvin (f t)

-- | Lifting function for functions that take two @Temperature@ values.
-- Used primarily to abstract away unit systems when performing temperature arithmetic.
liftT2 :: (Double -> Double -> Double) -> Temperature -> Temperature -> Temperature
liftT2 f (Fahrenheit t)    (Fahrenheit t') = Fahrenheit (f t t')
liftT2 f ut@(Fahrenheit _) ut'             = liftT2 f ut (toFahrenheit ut')

liftT2 f (Celsius t)       (Celsius t')    = Celsius    (f t t')
liftT2 f ut@(Celsius _)    ut'             = liftT2 f ut (toCelsius ut')

liftT2 f (Kelvin t)        (Kelvin t')     = Kelvin (f t t')
liftT2 f ut@(Kelvin _)     ut'             = liftT2 f ut (toKelvin ut')

-- | Converts any temperature value to Fahrenheit.
toFahrenheit :: Temperature -> Temperature
toFahrenheit (Fahrenheit t) = Fahrenheit t
toFahrenheit (Celsius t)    = Fahrenheit $ t * 1.8 + 32
toFahrenheit (Kelvin t)     = Fahrenheit $ (t - 273.15) * 1.8 + 32

-- | Converts any temperature value to Celsius.
toCelsius :: Temperature -> Temperature
toCelsius (Fahrenheit t) = Celsius $ (t - 32) * (5 / 9 :: Double)
toCelsius (Celsius t)    = Celsius t
toCelsius (Kelvin t)     = Celsius $ t - 273.15

-- | Converts any temperature value to Kelvin.
toKelvin :: Temperature -> Temperature
toKelvin (Fahrenheit t) = Kelvin $ (t - 32) * (5 / 9 :: Double) + 273.15
toKelvin (Celsius t)    = Kelvin $ t + 273.15
toKelvin (Kelvin t)     = Kelvin t

-- | Converts a @Double@ to a precipitation depth based on the provided @UnitSystem@.
toPrecipitation :: UnitSystem -> Double -> Precipitation
toPrecipitation Imperial p = Inch p
toPrecipitation _ p = Millimetre p

-- | Converts a @Double@ to a @Temperature@ based on the provided @UnitSystem@.
toTemperature :: UnitSystem -> Double -> Temperature
toTemperature Imperial t = Fahrenheit t
toTemperature Metric t   = Celsius t
toTemperature Standard t = Kelvin t

-- | Converts a @Double@ to a @Speed@ scalar based on the provided @UnitSystem@.
toSpeed :: UnitSystem -> Double -> Speed
toSpeed Imperial s = MilesPerHour s
toSpeed _ s = MetresPerSecond s

-- | Converts an @Integer@ to a @PresureLevel@ based on the provided @UnitSystem@.
toPressureLevel :: Integer -> PressureLevel
toPressureLevel p
  | p > 1014  = HighPressure
  | p < 1012  = LowPressure
  | otherwise = NormalPressure

-- | Gets the heat index of any @Temperature@ value in the same unit system.
-- Formula: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
toHeatIndex :: Temperature -> RelativeHumidity -> Temperature
toHeatIndex te (RelativeHumidity rhI) = case te of
  Fahrenheit _  ->             heatIndex te
  Celsius    _  -> toCelsius $ heatIndex te
  Kelvin     _  -> toKelvin  $ heatIndex te
  where
    rh = fromIntegral rhI
    heatIndex (Fahrenheit t)
      | adjHI < 80 = Fahrenheit $ 0.5 * (t + 61 + ((t - 68) * 1.2) + (rh * 0.094))
      | otherwise  = Fahrenheit adjHI
        where
          hi = -42.379 + 2.04901523*t + 10.14333127*rh - 0.22475541*t*rh - 0.00683783*t*t - 0.05481717*rh*rh + 0.00122874*t*t*rh + 0.00085282*t*rh*rh - 0.00000199*t*t*rh*rh
          adjHI
              | rh < 13 && t > 80 && t < 112 = hi - ((13 - rh) / 4) * sqrt ((17 - abs (t - 95)) / 17)
              | rh > 85 && t > 80 && t < 87 = hi + ((rh - 85) / 10) * ((87 - t) / 5)
              | otherwise = hi
    heatIndex t = heatIndex $ toFahrenheit t
