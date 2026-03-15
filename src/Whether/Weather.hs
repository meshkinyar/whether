{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Whether.Weather where

import Whether.Units
import Data.Time.Clock       ( UTCTime )
import Data.Time.Clock.POSIX ( POSIXTime )

-- | A common, processed subset of weather data for consumers.
data Forecast =
    DailyForecast
    { time             :: UTCTime
    -- location
    , sunset           :: UTCTime
    , sunrise          :: UTCTime
    , weatherCondition :: Maybe WeatherCondition
    , temperatureHigh  :: Temperature
    , temperatureLow   :: Temperature
    , humidity         :: RelativeHumidity
    , pressure         :: Pressure
    , windVelocity     :: Maybe WindVelocity
    , uvIndex          :: UVI
    , rainfall         :: Maybe Precipitation
    , snowfall         :: Maybe Precipitation
    , moon             :: Maybe MoonPhase
    } 
  | CurrentWeather
    { time             :: UTCTime
    -- location
    , sunset           :: UTCTime
    , sunrise          :: UTCTime
    , weatherCondition :: Maybe WeatherCondition
    , temperature      :: Temperature
    , humidity         :: RelativeHumidity
    , uvIndex          :: UVI
    , windVelocity     :: Maybe WindVelocity
    , moon             :: Maybe MoonPhase
    }

-- | Represents the phase of the moon at a given point in time.
data MoonPhase = NewMoon  | WaxingCrescent | FirstQuarter | WaxingGibbous
               | FullMoon | WaningGibbous  | LastQuarter  | WaningCrescent

-- | Represents the weather condition at a given point in time.
data WeatherCondition = Clear
                      | Cloudy   | MostlyCloudy | PartlyCloudy
                      | Rain     | RainPartial  | Thunderstorm | Tornado
                      | Fog      | Mist         | Haze         | Smoke 
                      | Snow     | Sleet
  deriving Eq

-- | Represents the velocity of the wind at a given point in time.
data WindVelocity = WindVelocity CardinalDirection Speed

-- | Type synonym for latitude / longitude values.
type Coordinates = (Double, Double)
