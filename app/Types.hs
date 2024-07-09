{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Types where

import Data.Aeson
import Data.Time.Clock.POSIX               ( POSIXTime )
import GHC.Generics                        ( Generic )
import Data.Text                           ( Text )

data TemperatureUnit = Kelvin | Celsius | Farenheit
    deriving Generic

instance FromJSON TemperatureUnit
instance ToJSON TemperatureUnit
instance Show TemperatureUnit where
    show u = case u of
        Kelvin    -> "standard"
        Celsius   -> "metric"
        Farenheit -> "imperial"

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
    deriving Generic

instance FromJSON WeatherCondition

instance Show WeatherCondition where
    show condition = case condition of
        ClearDay     -> "☀️ "
        ClearNight   -> "✨"
        Cloudy       -> "☁ "
        PartlyCloudy -> "🌤️"
        MostlyCloudy -> "🌥️"
        Rain         -> "🌧️"
        RainPartial  -> "🌦️"
        Thunderstorm -> "⛈️ "
        Tornado      -> "🌪️"
        Snow         -> "❄️ "
        Sleet        -> "🌨️"
        Fog          -> "🌫️"
        Mist         -> "🌁"
        Haze         -> "🌁"
        Smoke        -> "🔥"

data Temperature = T Double TemperatureUnit
    deriving Generic

instance Show Temperature where
    show (T t u) = show (round t :: Integer) ++ unit where
        unit = case u of
            Kelvin    -> "K"
            Celsius   -> "°C"
            Farenheit -> "°F"

type Coordinates = (Double, Double)

data Config = Config
    { apiKey :: Text
    , loc    :: Text
    , units  :: TemperatureUnit
    } 
    deriving Generic
instance FromJSON Config
instance ToJSON Config

type GeocodeRoot = [MatchedLocation]

data MatchedLocation = MatchedLocation
    { lat  :: Double
    , lon  :: Double
    } 
    deriving Generic
instance FromJSON MatchedLocation

data OneCallRoot = OneCallRoot
    { lat             :: Double
    , lon             :: Double
    , timezone        :: Text
    , timezone_offset :: Integer
    , current         :: Current
    , minutely        :: [Minute]
    , hourly          :: [Hour]
    , daily           :: [Day]
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
    , visibility :: Integer
    , wind_speed :: Double
    , wind_deg   :: Integer
    , wind_gust  :: Maybe Double
    , weather    :: [Weather]
    , rain       :: Maybe Precipitation
    , snow       :: Maybe Precipitation
    }
    deriving Generic
instance FromJSON Current
instance ToJSON Current

data Minute = Minute
    { dt            :: POSIXTime
    , precipitation :: Double
    } 
    deriving Generic
instance FromJSON Minute where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = label }
      where
        label "m_dt" = "dt"
        label x      = x
instance ToJSON Minute

data Hour = Hour
    { dt         :: POSIXTime
    , temp       :: Double
    , feels_like :: Double
    , pressure   :: Integer
    , humidity   :: Integer
    , dew_point  :: Double
    , uvi        :: Float
    , clouds     :: Integer
    , visibility :: Integer
    , wind_speed :: Double
    , wind_deg   :: Integer
    , wind_gust  :: Maybe Double
    , weather    :: [Weather]
    , pop        :: Double
    , rain       :: Maybe Precipitation
    , snow       :: Maybe Precipitation
    }
    deriving Generic
instance FromJSON Hour
instance ToJSON Hour

data Day = Day
    { dt            :: POSIXTime
    , sunrise       :: POSIXTime
    , sunset        :: POSIXTime
    , moonrise      :: POSIXTime
    , moonset       :: POSIXTime
    , moon_phase    :: Double
    , summary       :: Text
    , d_temp        :: DayTemp
    , d_feels_like  :: DayFeelsLike
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
instance FromJSON Day where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _day }
instance ToJSON Day where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = _day }

newtype Precipitation = Precipitation
    { oneH :: Double }
    deriving Generic
instance FromJSON Precipitation where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _precipitation }
instance ToJSON Precipitation where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = _precipitation }

data DayTemp = DayTemp
    { day   :: Double
    , min   :: Double
    , max   :: Double
    , night :: Double
    , eve   :: Double
    , morn  :: Double
    }
    deriving Generic
instance FromJSON DayTemp
instance ToJSON DayTemp

data DayFeelsLike = DayFeelsLike
    { day   :: Double
    , night :: Double
    , eve   :: Double
    , morn  :: Double
    }
    deriving Generic
instance FromJSON DayFeelsLike
instance ToJSON DayFeelsLike

data Weather = Weather
    { weather_id  :: Integer
    , main        :: Text
    , description :: Text
    , icon        :: Text
    }
    deriving Generic

instance FromJSON Weather where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _weather }
instance ToJSON Weather where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = _weather }

-- Label Functions

_day :: String -> String
_day "d_temp" = "temp"
_day "d_feels_like" = "feels_like"
_day "d_rain" = "rain"
_day "d_snow" = "snow"
_day x = x

_weather :: String -> String
_weather "weather_id" = "id"
_weather s = s

_precipitation :: String -> String
_precipitation "oneH" = "1h"
_precipitation x = x
