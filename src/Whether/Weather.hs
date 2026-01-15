{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Whether.Weather where

import Whether.Units
import Data.Time.Clock       ( UTCTime )
import Data.Time.Clock.POSIX ( POSIXTime )

data Forecast =
    DailyForecast
    { time             :: UTCTime
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
    { weatherCondition :: Maybe WeatherCondition
    , temperature      :: Temperature
    , humidity         :: RelativeHumidity
    , moon             :: Maybe MoonPhase
    }

data MoonPhase = NewMoon  | WaxingCrescent | FirstQuarter | WaxingGibbous
               | FullMoon | WaningGibbous  | LastQuarter  | WaningCrescent

data WeatherCondition = Clear
                      | Cloudy   | MostlyCloudy | PartlyCloudy
                      | Rain     | RainPartial  | Thunderstorm | Tornado
                      | Fog      | Mist         | Haze         | Smoke 
                      | Snow     | Sleet
  deriving Eq

data WindVelocity = WindVelocity CardinalDirection Speed

type Coordinates = (Double, Double)
