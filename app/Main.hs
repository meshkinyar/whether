{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

module Main where

import Data.Aeson
import Network.HTTP.Simple
import Data.Ix                             ( inRange )
import Control.Monad                       ( when )
import Data.Time.Clock.POSIX               ( getPOSIXTime, utcTimeToPOSIXSeconds, POSIXTime )
import GHC.Generics                        ( Generic )
import Data.Maybe                          ( fromJust )
import Data.Text                           ( Text, pack )
import Data.Text.Encoding                  ( encodeUtf8 )
import Data.ByteString                     ( ByteString )
import System.Directory                    ( createDirectoryIfMissing, doesFileExist, getModificationTime )
import qualified Data.ByteString.Lazy as L ( ByteString )

data TemperatureUnit = Kelvin | Celsius | Farenheit
    deriving Generic

instance FromJSON TemperatureUnit
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
                      | Snow     | Sleet
                      | Fog      | Mist         | Haze         | Smoke 
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
    } deriving Generic
instance FromJSON Config

type GeocodeRoot = [MatchedLocation]

data MatchedLocation = MatchedLocation
    { lat :: Double
    , lon :: Double
    } deriving Generic
instance FromJSON MatchedLocation

data OneCallRoot = OneCallRoot
    { current :: Current
    , daily   :: [Day]
    } deriving Generic
instance FromJSON OneCallRoot
instance ToJSON OneCallRoot

data Current = Current
    { 
      dt       :: POSIXTime
    , sunrise  :: POSIXTime
    , sunset   :: POSIXTime
    , temp     :: Double
    , humidity :: Integer
    , weather  :: [Weather]
    } deriving Generic
instance FromJSON Current
instance ToJSON Current

newtype Day = Day
    { moon_phase :: Double }
    deriving Generic
instance FromJSON Day
instance ToJSON Day

newtype Weather = Weather
    { weather_id :: Integer }
    deriving Generic

instance FromJSON Weather where
    parseJSON = genericParseJSON ( defaultOptions { fieldLabelModifier = weatherField } )
      where
        weatherField "weather_id" = "id"
        weatherField s = s
instance ToJSON Weather where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = weatherField }
      where
        weatherField "weather_id" = "id"
        weatherField s = s

main :: IO ()
main = do
    config  <- getConfig 
    oneCall <- getOneCall config
    cacheOneCall cachePath oneCall
    formatOutput config oneCall

getConfig :: IO Config
getConfig = do 
    configFile <- decodeFileStrict "config.json" :: IO (Maybe Config)
    case configFile of
        Nothing -> error "Invalid config.json"
        Just x  -> return x

getOneCall :: Config -> IO OneCallRoot
getOneCall cfg = do
    cacheFile <- decodeFileStrictIfExists cachePath :: IO (Maybe OneCallRoot)
    lastModification <- getModificationTime "config.json"
    useCache <- cacheValid cacheFile (utcTimeToPOSIXSeconds lastModification) <$> getPOSIXTime

    if useCache
    then do
        return $ fromJust cacheFile
    else do
        geocodeResponse  <- callAPI (apiKey cfg) 
                         $  geocodeRequest
                         $  loc cfg
        geocode          <- maybe  (error "Invalid Geocoding Response")
                                   return (decode geocodeResponse :: Maybe GeocodeRoot)
        let location     = case geocode of
                                 []    -> error "Empty Response"
                                 (x:_) -> x
        oneCallResponse  <- callAPI (apiKey cfg)
                          $ oneCallRequest (lat location, lon location)
                          $ units cfg
        maybe (error "Invalid One Call 3.0 API Response")
              return (decode oneCallResponse :: Maybe OneCallRoot)

formatOutput :: Config -> OneCallRoot -> IO ()
formatOutput config oneCall = do
    let weatherIcon  = toWeatherCondition (current oneCall)
                     $ weather_id
                     $ case weatherList of
                           []    -> error "Empty response"
                           (x:_) -> x
                       where weatherList = weather $ current oneCall
    let temperature  = T (temp $ current oneCall) 
                     $ units config
    let rH           = humidity $ current oneCall
    let moonPhase    = toMoonPhase
                     $ moon_phase
                     $ case dayList of
                           []    -> error "Empty response"
                           (x:_) -> x
                       where dayList = daily oneCall

    putStr . show $ weatherIcon
    putStr " "
    putStr . show $ temperature
    putStr " 💧 "
    putStr . show $ rH
    putStr "% "
    putStr " "
    putStr . show $ moonPhase

callAPI :: Text -> Request -> IO L.ByteString
callAPI key requestName = do
    let r = addToRequestQueryString [("appid", Just $ encodeUtf8 key)] requestName
    response <- httpLBS r
    return ( getResponseBody response )

decodeFileStrictIfExists :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeFileStrictIfExists path = do
    exists <- doesFileExist path
    if exists 
    then decodeFileStrict cachePath
    else return Nothing

cacheOneCall :: (ToJSON a) => FilePath -> a -> IO ()
cacheOneCall path oc = do
    createDirectoryIfMissing True ".cache"
    exists <- doesFileExist path
    when exists $ encodeFile cachePath oc
-- Pure Functions --

---- Constants
cachePath :: FilePath
cachePath = ".cache/cache.json"

---- Conversions
toMoonPhase :: Double -> MoonPhase
toMoonPhase x | x `elem` [0, 1]          = NewMoon
              | (x > 0   ) && (x < 0.25) = WaxingCrescent
              | x == 0.25                = FirstQuarter
              | (x > 0.25) && (x < 0.5)  = WaxingGibbous
              | x == 0.50                = FullMoon
              | (x > 0.50) && (x < 0.75) = WaningGibbous
              | x == 0.75                = LastQuarter
              | (x > 0.75) && (x < 1.00) = WaningCrescent
              | otherwise                = error "Invalid value for Moon Phase"

toWeatherCondition :: Current -> Integer -> WeatherCondition
toWeatherCondition c x |  inRange  (200, 299) x  = Thunderstorm
                       |  inRange  (300, 399) x
                       || inRange  (502, 599) x  = Rain
                       |  inRange  (500, 502) x  = RainPartial
                       |  inRange  (600, 699) x  = Snow
                       |  x == 701               = Mist
                       |  x == 711               = Smoke
                       |  x `elem` [721, 731,
                                    751, 761]    = Haze
                       |  x == 741               = Fog
                       |  x == 781               = Tornado
                       |  x == 800 && isDay c    = ClearDay
                       |  x == 800               = ClearNight
                       |  x == 801               = PartlyCloudy
                       |  x `elem` [802, 803]    = MostlyCloudy
                       |  x == 804               = Cloudy
                       |  otherwise              = error "Weather Condition id is out of range"

formatCoord :: Double -> ByteString
formatCoord x = encodeUtf8 $ pack $ show x

formatUnits :: TemperatureUnit -> ByteString
formatUnits x = encodeUtf8 $ pack $ show x

---- Simple HTTP Request
owmRequest :: Request
owmRequest =
    setRequestMethod "GET"
  $ setRequestSecure True
  $ setRequestPort 443
  $ setRequestHost "api.openweathermap.org"
    defaultRequest

geocodeRequest :: Text -> Request
geocodeRequest location =
    setRequestPath "/geo/1.0/direct" 
  $ setRequestQueryString [ ("q", Just $ encodeUtf8 location)
                          , ("limit", Just "1")
                          ]
    owmRequest

oneCallRequest :: Coordinates -> TemperatureUnit -> Request
oneCallRequest (latitude, longitude) unit = 
    setRequestPath "/data/3.0/onecall"
  $ setRequestQueryString [ ("lat"  , Just $ formatCoord latitude)
                          , ("lon"  , Just $ formatCoord longitude)
                          , ("units", Just $ formatUnits unit)
                          ]
    owmRequest

---- Miscellaneous Helpers
isDay :: Current -> Bool
isDay c = dt c >= sunrise c

cacheValid :: Maybe OneCallRoot -> POSIXTime -> POSIXTime -> Bool
cacheValid cache modT t = 
    case cache of
        Nothing -> False
        Just x  -> modT  <= dt (current x)
                && t     <= dt (current x) + 600
