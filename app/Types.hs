{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Types where

import Data.Aeson
import Data.Time.Clock.POSIX    ( POSIXTime )
import Data.Time.Clock          ( UTCTime )
import GHC.Generics             ( Generic )
import qualified Data.Text as S ( Text )

data CardinalDirection = NorthWest | North | NorthEast
                       | West              | East
                       | SouthWest | South | SouthEast

data UnitSystem = Standard | Metric | Imperial
    deriving (Generic, Read, Show)
instance FromJSON UnitSystem
instance ToJSON UnitSystem

-- Note: OWM only specifies precipitation in millimeters
data Precipitation = Inch Double | Millimetre Double

data Speed = MilesPerHour Double | MetresPerSecond Double

data MoonPhase = NewMoon  | WaxingCrescent | FirstQuarter | WaxingGibbous
               | FullMoon | WaningGibbous  | LastQuarter  | WaningCrescent
    deriving Generic

-- Weather Conditions with Unicode Representations
data WeatherCondition = Clear
                      | Cloudy   | MostlyCloudy | PartlyCloudy
                      | Rain     | RainPartial  | Thunderstorm | Tornado
                      | Fog      | Mist         | Haze         | Smoke 
                      | Snow     | Sleet
    deriving (Generic, Eq)

data Temperature = Kelvin Double | Celsius Double | Fahrenheit Double
    deriving Generic

newtype RelativeHumidity = RelativeHumidity Integer
newtype Pressure = Pressure Double

newtype UVI = UVI Double
  deriving Generic
instance FromJSON UVI

data WindVelocity = WindVelocity CardinalDirection Speed
    deriving Generic

data PressureLevel = HighPressure | NormalPressure | LowPressure

instance FromJSON WeatherCondition

type Coordinates = (Double, Double)

-- Wrapper for config file metadata
data Config = Config
    { apiKey     :: S.Text
    , loc        :: S.Text
    , unitSystem :: UnitSystem
    }
    deriving Generic

instance FromJSON Config
instance ToJSON Config

type GeocodeRoot = [MatchedLocation]

data MatchedLocation = MatchedLocation
    { name    :: S.Text
    , lat     :: Double
    , lon     :: Double
    , country :: S.Text
    } 
    deriving Generic
instance FromJSON MatchedLocation
instance ToJSON MatchedLocation

data OneCallRoot = OneCallRoot
    { lat             :: Double
    , lon             :: Double
    , timezone        :: S.Text
    , timezone_offset :: Integer
    , current         :: Current
    , minutely        :: Maybe [Minutely]
    , hourly          :: [Hourly]
    , daily           :: [Daily]
    , alerts          :: Maybe [Alert]
    } 
    deriving Generic
instance FromJSON OneCallRoot
instance ToJSON OneCallRoot

data Current = Current
    { dt         :: POSIXTime
    , sunrise    :: POSIXTime
    , sunset     :: POSIXTime
    , c_temp       :: Double
    , feels_like :: Double
    , pressure   :: Double
    , humidity   :: Integer 
    , dew_point  :: Double
    , uvi        :: Double
    , clouds     :: Integer
    , visibility :: Maybe Integer
    , wind_speed :: Double
    , wind_deg   :: Integer
    , wind_gust  :: Maybe Double
    , weather    :: [Weather]
    , rain       :: Maybe Precip1h
    , snow       :: Maybe Precip1h
    }
    deriving Generic
instance FromJSON Current where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _current }
instance ToJSON Current where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = _current }

data Minutely = Minutely
    { dt            :: POSIXTime
    , precipitation :: Double
    } 
    deriving Generic
instance FromJSON Minutely where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = label }
      where
        label "m_dt" = "dt"
        label x      = x
instance ToJSON Minutely

data Hourly = Hourly
    { dt         :: POSIXTime
    , h_temp     :: Double
    , feels_like :: Double
    , pressure   :: Double
    , humidity   :: Integer
    , dew_point  :: Double
    , uvi        :: Double
    , clouds     :: Integer
    , visibility :: Maybe Integer
    , wind_speed :: Double
    , wind_deg   :: Integer
    , wind_gust  :: Maybe Double
    , weather    :: [Weather]
    , pop        :: Double
    , rain       :: Maybe Precip1h
    , snow       :: Maybe Precip1h
    }
    deriving Generic
instance FromJSON Hourly where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _hourly }
instance ToJSON Hourly where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = _hourly }

newtype Precip1h = Precip1h
    { oneH :: Double }
    deriving Generic

instance FromJSON Precip1h where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _precip1h }
instance ToJSON Precip1h where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = _precip1h }

data Daily = Daily
    { dt            :: POSIXTime
    , sunrise       :: POSIXTime
    , sunset        :: POSIXTime
    , moonrise      :: POSIXTime
    , moonset       :: POSIXTime
    , moon_phase    :: Double
    , summary       :: S.Text
    , d_temp        :: DailyTemp
    , d_feels_like  :: DailyFeelsLike
    , pressure      :: Double
    , humidity      :: Integer
    , dew_point     :: Double
    , wind_speed    :: Double
    , wind_deg      :: Integer
    , wind_gust     :: Double
    , weather       :: [Weather]
    , clouds        :: Integer
    , pop           :: Double
    , d_rain        :: Maybe Double
    , d_snow        :: Maybe Double
    , uvi           :: Double
    }
    deriving Generic
instance FromJSON Daily where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _daily }
instance ToJSON Daily where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = _daily }

data DailyTemp = DailyTemp
    { day   :: Double
    , min   :: Double
    , max   :: Double
    , night :: Double
    , eve   :: Double
    , morn  :: Double
    }
    deriving Generic
instance FromJSON DailyTemp
instance ToJSON DailyTemp

data DailyFeelsLike = DailyFeelsLike
    { day   :: Double
    , night :: Double
    , eve   :: Double
    , morn  :: Double
    }
    deriving Generic
instance FromJSON DailyFeelsLike
instance ToJSON DailyFeelsLike

data Weather = Weather
    { weather_id   :: Integer
    , weather_main :: S.Text
    , description  :: S.Text
    , icon         :: S.Text
    }
    deriving Generic

instance FromJSON Weather where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _weather }
instance ToJSON Weather where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = _weather }

data Alert = Alert
    { sender_name :: S.Text
    , event       :: S.Text
    , start       :: POSIXTime
    , end         :: POSIXTime
    , description :: S.Text
    , tags        :: [S.Text]
    }
    deriving Generic

instance FromJSON Alert
instance ToJSON Alert

-- Encoding

data Forecast =
    DailyForecast
    { time             :: UTCTime
    , sunset           :: UTCTime
    , sunrise          :: UTCTime
    , weatherCondition :: Maybe WeatherCondition
    , temperatureRange :: (Temperature, Temperature)
    , humidity         :: RelativeHumidity
    , pressure         :: Pressure
    , windVelocity     :: Maybe WindVelocity
    , uvIndex          :: UVI
    , rainfall         :: Maybe Precipitation
    , snowfall         :: Maybe Precipitation
    , moon             :: Maybe MoonPhase
    } 
  | CurrentWeather
    { condition   :: Maybe WeatherCondition
    , temperature :: Temperature
    , humidity    :: RelativeHumidity
    , moon        :: Maybe MoonPhase
    }

-- Label Functions

_daily :: String -> String
_daily "d_temp" = "temp"
_daily "d_feels_like" = "feels_like"
_daily "d_rain" = "rain"
_daily "d_snow" = "snow"
_daily x = x

_hourly :: String -> String
_hourly "h_temp" = "temp"
_hourly x = x

_current :: String -> String
_current "c_temp" = "temp"
_current x = x

_weather :: String -> String
_weather "weather_id" = "id"
_weather "weather_main" = "main"
_weather s = s

_precip1h :: String -> String
_precip1h "oneH" = "1h"
_precip1h x = x
