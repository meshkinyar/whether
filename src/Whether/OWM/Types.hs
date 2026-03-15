{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}

module Whether.OWM.Types where

import Optics
import GHC.Generics             ( Generic )
import Data.Ix
import Data.Aeson
import Data.Time.Clock          ( UTCTime )
import Data.Time.Clock.POSIX    ( POSIXTime, posixSecondsToUTCTime )
import qualified Data.Text as S ( Text )

import Whether.Config
import Whether.Units
import Whether.Weather

-- | Type synonym for a 2D point of Double values.
type Coordinates = (Double, Double)

-- | The root object returned by the Geocoding API.
type GeocodeRoot = [MatchedLocation]

-- | Data matched to the provided location string.
data MatchedLocation = MatchedLocation
  { name    :: S.Text
  , lat     :: Double
  , lon     :: Double
  , country :: S.Text
  } 
  deriving Generic
instance FromJSON MatchedLocation
instance ToJSON MatchedLocation

-- | The root object returned by the OneCall 3.0 API.
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

-- | Object containing the current weather.
data Current = Current
  { dt         :: POSIXTime
  , sunrise    :: POSIXTime
  , sunset     :: POSIXTime
  , temp       :: Double
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

instance FromJSON Current
instance ToJSON Current

-- | Object containing a given minute's forecast.
data Minutely = Minutely
  { dt            :: POSIXTime
  , precipitation :: Double
  } 
  deriving Generic
instance FromJSON Minutely
instance ToJSON Minutely

-- | Object containing a given hour's forecast.
data Hourly = Hourly
  { dt         :: POSIXTime
  , temp       :: Double
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
instance FromJSON Hourly
instance ToJSON Hourly

-- | Object containing the precipitation expected over one hour.
newtype Precip1h = Precip1h
  { oneH :: Double }
  deriving Generic

instance FromJSON Precip1h where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = _precip1h }
instance ToJSON Precip1h where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = _precip1h }

_precip1h :: String -> String
_precip1h "oneH" = "1h"
_precip1h x = x

-- | Object containing a given day's forecast.
data Daily = Daily
  { dt            :: POSIXTime
  , sunrise       :: POSIXTime
  , sunset        :: POSIXTime
  , moonrise      :: POSIXTime
  , moonset       :: POSIXTime
  , moon_phase    :: Double
  , summary       :: S.Text
  , temp          :: DailyTemp
  , feels_like    :: DailyFeelsLike
  , pressure      :: Double
  , humidity      :: Integer
  , dew_point     :: Double
  , wind_speed    :: Double
  , wind_deg      :: Integer
  , wind_gust     :: Double
  , weather       :: [Weather]
  , clouds        :: Integer
  , pop           :: Double
  , rain          :: Maybe Double
  , snow          :: Maybe Double
  , uvi           :: Double
  }
  deriving Generic
instance FromJSON Daily
instance ToJSON Daily

-- | Object containing temperature data for a given day.
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

-- | Object containing feels like temperature data for a given day.
data DailyFeelsLike = DailyFeelsLike
  { day   :: Double
  , night :: Double
  , eve   :: Double
  , morn  :: Double
  }
  deriving Generic
instance FromJSON DailyFeelsLike
instance ToJSON DailyFeelsLike

-- | Object containing data on the weather condition.
data Weather = Weather
  { id          :: Integer
  , main        :: S.Text
  , description :: S.Text
  , icon        :: S.Text
  }
  deriving Generic

instance FromJSON Weather
instance ToJSON Weather

-- | Object containing data on any active weather alerts in the area.
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

---- Conversions ----

-- | Converts the OWM moon_phase float to a defined MoonPhase type.
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

-- | Converts the OWM weather condition code to a defined type.
toWeatherCondition :: Integer -> Maybe WeatherCondition
toWeatherCondition x
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
  |  x == 800               = Just Clear
  |  x == 801               = Just PartlyCloudy
  |  x `elem` [802, 803]    = Just MostlyCloudy
  |  x == 804               = Just Cloudy
  |  otherwise              = Nothing

-- | Checks whether the time of day is between sunrise and sunset.
isDayCurrent :: Current -> Bool
isDayCurrent cur = cur ^. #dt >= cur ^. #sunrise && cur ^. #dt <= cur ^. #sunset

-- | Converts the angle of the wind (in degrees) to a cardinal direction.
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

toWindVelocity :: UnitSystem -> Integer -> Double -> Maybe WindVelocity
toWindVelocity units deg s = do
  dir <- toCardinalDirection deg
  return $ WindVelocity dir (toSpeed units s)

-- | Creates a daily forecast from a Daily object in a OneCall API response.
mkDailyForecast :: UnitSystem -> Daily -> Forecast
mkDailyForecast units day = DailyForecast
  { time             = posixSecondsToUTCTime $ day ^. #dt
  , sunrise          = posixSecondsToUTCTime $ day ^. #sunrise
  , sunset           = posixSecondsToUTCTime $ day ^. #sunset
  , weatherCondition = getFirstWeather       $ day ^. #weather
  , temperatureLow   = toTemperature units   $ day ^. #temp % #min
  , temperatureHigh  = toTemperature units   $ day ^. #temp % #max
  , humidity         = RelativeHumidity      $ day ^. #humidity
  , pressure         = HectoPascal           $ day ^. #pressure
  , windVelocity     = windF                 $ day ^. #wind_deg
  , uvIndex          = UVI                   $ day ^. #uvi 
  , rainfall         = toPrecip              $ day ^. #rain
  , snowfall         = toPrecip              $ day ^. #snow
  , moon             = toMoonPhase           $ day ^. #moon_phase
  }
  where
    toPrecip field = do toPrecipitation units <$> field
    windF direction = do
      dir <- toCardinalDirection direction
      return $ WindVelocity dir (toSpeed units $ day ^. #wind_speed)

-- | Gets a daily forecast for each day in a OneCall API response.
getDailyForecasts :: Config -> OneCallRoot -> [Forecast]
getDailyForecasts config oneCall = map (mkDailyForecast (config ^. #unitSystem)) $ daily oneCall

-- | Gets the current weather reading from a OneCall API response.
getCurrentWeather :: Config -> OneCallRoot -> Forecast
getCurrentWeather config oneCall = CurrentWeather
  { time             = posixSecondsToUTCTime               $ cur ^. #dt
  , sunrise          = posixSecondsToUTCTime               $ cur ^. #sunrise
  , sunset           = posixSecondsToUTCTime               $ cur ^. #sunset
  , weatherCondition = getFirstWeather                     $ cur ^. #weather
  , temperature      = toTemperature u                     $ cur ^. #temp
  , humidity         = RelativeHumidity                    $ cur ^. #humidity
  , uvIndex          = UVI                                 $ cur ^. #uvi
  , windVelocity     = toWindVelocity u (cur ^. #wind_deg) $ cur ^. #wind_speed
  , moon             = case daily oneCall of
    x:_ -> toMoonPhase $ moon_phase x
    []  -> Nothing
  }
  where
    u   = config ^. #unitSystem
    cur = oneCall ^. #current

-- | Helper to get the first weather condition ID from a list of @(Weather) objects.
getFirstWeather :: [Weather] -> Maybe WeatherCondition
getFirstWeather (x:_) = toWeatherCondition $ x ^. #id
getFirstWeather [] = Nothing
