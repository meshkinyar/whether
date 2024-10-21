{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Types where

import Data.Aeson
import Data.Time.Clock.POSIX ( POSIXTime )
import GHC.Generics          ( Generic )
import Data.Text             ( Text )

data CardinalDirection = NorthWest | North | NorthEast
                       | West              | East
                       | SouthWest | South | SouthEast

-- Note: OWM only specifies precipitation in millimeters

data UnitSystem = Standard | Metric | Imperial
    deriving (Generic, Read, Show)
instance FromJSON UnitSystem
instance ToJSON UnitSystem

data Precipitation = Inch Double | Millimetre Double

data MoonPhase = NewMoon  | WaxingCrescent | FirstQuarter | WaxingGibbous
               | FullMoon | WaningGibbous  | LastQuarter  | WaningCrescent
    deriving Generic

instance Show MoonPhase where
    show phase = case phase of
        NewMoon        -> "🌑"
        WaxingCrescent -> "🌒"
        FirstQuarter   -> "🌓"
        WaxingGibbous  -> "🌔"
        FullMoon       -> "🌕"
        WaningGibbous  -> "🌖"
        LastQuarter    -> "🌗"
        WaningCrescent -> "🌘"

-- Weather Conditions with Unicode Representations
data WeatherCondition = ClearDay | ClearNight
                      | Cloudy   | MostlyCloudy | PartlyCloudy
                      | Rain     | RainPartial  | Thunderstorm | Tornado
                      | Fog      | Mist         | Haze         | Smoke 
                      | Snow     | Sleet
    deriving (Generic, Eq)

instance FromJSON WeatherCondition

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

data Temperature = T Double UnitSystem
    deriving Generic

data Wind = Wind Double UnitSystem CardinalDirection
    deriving Generic

instance Show Temperature where
    show (T t u) = show (round t :: Integer) ++ unit where
        unit = case u of
            Standard -> "K"
            Metric   -> "°C"
            Imperial -> "°F"

type Coordinates = (Double, Double)

-- Wrapper for config file metadata
data Config = Config
    { apiKey     :: Text
    , loc        :: Text
    , unitSystem :: UnitSystem
    }
    deriving Generic

instance FromJSON Config
instance ToJSON Config

type GeocodeRoot = [MatchedLocation]

data MatchedLocation = MatchedLocation
    { name    :: Text
    , lat     :: Double
    , lon     :: Double
    , country :: Text
    } 
    deriving Generic
instance FromJSON MatchedLocation
instance ToJSON MatchedLocation

data OneCallRoot = OneCallRoot
    { lat             :: Double
    , lon             :: Double
    , timezone        :: Text
    , timezone_offset :: Integer
    , current         :: Current
    , minutely        :: [Minutely]
    , hourly          :: [Hourly]
    , daily           :: [Daily]
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
    , rain       :: Maybe Double
    , snow       :: Maybe Double
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
    , rain       :: Maybe Double
    , snow       :: Maybe Double
    }
    deriving Generic
instance FromJSON Hourly
instance ToJSON Hourly

data Daily = Daily
    { dt            :: POSIXTime
    , sunrise       :: POSIXTime
    , sunset        :: POSIXTime
    , moonrise      :: POSIXTime
    , moonset       :: POSIXTime
    , moon_phase    :: Double
    , summary       :: Text
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
    , weather_main :: Text
    , description  :: Text
    , icon         :: Text
    }
    deriving Generic

instance FromJSON Weather where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _weather }
instance ToJSON Weather where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = _weather }

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

_precipitation :: String -> String
_precipitation "oneH" = "1h"
_precipitation x = x
