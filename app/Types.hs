{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Types where

import Data.Aeson
import Data.Time.Clock.POSIX    ( POSIXTime )
import GHC.Generics             ( Generic )
import qualified Data.Text as T ( Text )

data CardinalDirection = NorthWest | North | NorthEast
                       | West              | East
                       | SouthWest | South | SouthEast

instance Show CardinalDirection where
    show d = case d of
        NorthWest -> "Northwest"
        North     -> "North"
        NorthEast -> "Northeast"
        West      -> "West"
        East      -> "East"
        SouthWest -> "Southwest"
        South     -> "South"
        SouthEast -> "Southeast"

-- Note: OWM only specifies precipitation in millimeters

data UnitSystem = Standard | Metric | Imperial
    deriving (Generic, Read, Show)
instance FromJSON UnitSystem
instance ToJSON UnitSystem

data Precipitation = Inch Double | Millimetre Double

data MoonPhase = NewMoon  | WaxingCrescent | FirstQuarter | WaxingGibbous
               | FullMoon | WaningGibbous  | LastQuarter  | WaningCrescent
    deriving Generic

-- Weather Conditions with Unicode Representations
data WeatherCondition = ClearDay | ClearNight
                      | Cloudy   | MostlyCloudy | PartlyCloudy
                      | Rain     | RainPartial  | Thunderstorm | Tornado
                      | Fog      | Mist         | Haze         | Smoke 
                      | Snow     | Sleet
    deriving (Generic, Eq)

instance FromJSON WeatherCondition

instance Show MoonPhase where
    show phase = case phase of
        NewMoon        -> "New Moon"
        WaxingCrescent -> "Waxing Crescent"
        FirstQuarter   -> "First Quarter"
        WaxingGibbous  -> "Waxing Gibbous"
        FullMoon       -> "Full Moon"
        WaningGibbous  -> "Waning Gibbous"
        LastQuarter    -> "Third Quarter"
        WaningCrescent -> "Waning Crescent"


instance Show WeatherCondition where
    show condition = case condition of
        ClearDay     -> "Clear"
        ClearNight   -> "Clear"
        Cloudy       -> "Cloudy"
        PartlyCloudy -> "Partly Cloudy"
        MostlyCloudy -> "Mostly Cloudy"
        Rain         -> "Rain"
        RainPartial  -> "Partial Rain"
        Thunderstorm -> "Thunderstorm"
        Tornado      -> "Tornado"
        Snow         -> "Snow"
        Sleet        -> "Sleet"
        Fog          -> "Fog"
        Mist         -> "Mist"
        Haze         -> "Haze"
        Smoke        -> "Smoke"

data Temperature = T UnitSystem Double
    deriving Generic

data Wind = Wind UnitSystem CardinalDirection Double
    deriving Generic

instance Show Temperature where
    show (T u t) = show (round t :: Integer) ++ unit where
        unit = case u of
            Standard -> "K"
            Metric   -> "°C"
            Imperial -> "°F"

type Coordinates = (Double, Double)

-- Wrapper for config file metadata
data Config = Config
    { apiKey     :: T.Text
    , loc        :: T.Text
    , unitSystem :: UnitSystem
    }
    deriving Generic

instance FromJSON Config
instance ToJSON Config

type GeocodeRoot = [MatchedLocation]

data MatchedLocation = MatchedLocation
    { name    :: T.Text
    , lat     :: Double
    , lon     :: Double
    , country :: T.Text
    } 
    deriving Generic
instance FromJSON MatchedLocation
instance ToJSON MatchedLocation

data OneCallRoot = OneCallRoot
    { lat             :: Double
    , lon             :: Double
    , timezone        :: T.Text
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
    , temp       :: Double
    , feels_like :: Double
    , pressure   :: Integer
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
instance FromJSON Current
instance ToJSON Current

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
    , temp       :: Double
    , feels_like :: Double
    , pressure   :: Integer
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
instance FromJSON Hourly
instance ToJSON Hourly

data Precip1h = Precip1h
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
    , summary       :: T.Text
    , d_temp        :: DailyTemp
    , d_feels_like  :: DailyFeelsLike
    , pressure      :: Integer
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
    , weather_main :: T.Text
    , description  :: T.Text
    , icon         :: T.Text
    }
    deriving Generic

instance FromJSON Weather where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _weather }
instance ToJSON Weather where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = _weather }

data Alert = Alert
    { sender_name :: T.Text
    , event       :: T.Text
    , start       :: POSIXTime
    , end         :: POSIXTime
    , description :: T.Text
    , tags        :: [T.Text]
    }
    deriving Generic

instance FromJSON Alert
instance ToJSON Alert

-- Encoding

data DailyForecast = DailyForecast
    { time  :: POSIXTime
    , cond  :: Maybe WeatherCondition
    , temps :: (Temperature, Temperature)
    , rH    :: Integer
    , hPa   :: Integer
    , wind  :: Maybe Wind
    , uvi   :: Double
    , rain  :: Maybe Precipitation
    , snow  :: Maybe Precipitation
    }

data CurrentWeather = CurrentWeather
    { cond :: Maybe WeatherCondition
    , temp :: Temperature
    , rH   :: Integer
    , moon :: Maybe MoonPhase
    }

data PressureLevel = HighPressure | NormalPressure | LowPressure

-- Label Functions

_daily :: String -> String
_daily "d_temp" = "temp"
_daily "d_feels_like" = "feels_like"
_daily "d_rain" = "rain"
_daily "d_snow" = "snow"
_daily x = x

_weather :: String -> String
_weather "weather_id" = "id"
_weather "weather_main" = "main"
_weather s = s

_precip1h :: String -> String
_precip1h "oneH" = "1h"
_precip1h x = x
