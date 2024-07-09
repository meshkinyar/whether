{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Main where

import Data.Aeson
import Network.HTTP.Simple
import Data.Ix                             ( inRange )
import Control.Monad                       ( unless )
import Data.Time.Clock.POSIX               ( getPOSIXTime, utcTimeToPOSIXSeconds, POSIXTime )
import Data.Maybe                          ( fromJust )
import Data.Text                           ( Text, pack )
import Data.Text.Encoding                  ( encodeUtf8 )
import Data.ByteString                     ( ByteString )
import System.Directory                    ( createDirectoryIfMissing, doesFileExist, getModificationTime )
import Types
import qualified Data.ByteString.Lazy as L ( ByteString )

-- Duplicate Fields --
import qualified Types as G                ( MatchedLocation(lat, lon) )
import qualified Types as R                ( OneCallRoot(lat, lon) )
import qualified Types as C                ( Current(dt, sunrise, sunset, temp, feels_like, pressure
                                           , humidity, dew_point, uvi, clouds, visibility
                                           , wind_speed, wind_deg, wind_gust, weather)
                                           )
import qualified Types as M                ( Minute(dt) )
import qualified Types as H                ( Hour(dt, temp, feels_like, pressure, humidity, dew_point
                                           , uvi, clouds, visibility, wind_speed, wind_gust, weather, pop)
                                           )  
import qualified Types as D                ( Day(dt, sunrise, sunset, pressure, dew_point, humidity
                                           , wind_speed, wind_deg, wind_gust, weather, clouds, pop, uvi)
                                           )


main :: IO ()
main = do
    config  <- getConfig 
    oneCall <- getOneCall config
    formatOutput config oneCall

getConfig :: IO Config
getConfig = do 
    configExists <- doesFileExist "config.json"
    unless configExists $ encodeFile "config.json" defaultConfig 
                        >> putStrLn "No config.json found, creating default config"
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
        geocode          <- maybe  (error "Invalid Geocoding Response. Ensure that your API key in config.json is valid.")
                                   return (decode geocodeResponse :: Maybe GeocodeRoot)
        let location     = case geocode of
                                 []    -> error "Empty Response"
                                 (x:_) -> x
        oneCallResponse  <- callAPI (apiKey cfg)
                          $ oneCallRequest (G.lat location, G.lon location)
                          $ units cfg
        case (eitherDecode oneCallResponse :: Either String OneCallRoot) of
            Left  x   -> error x
            Right x   -> cacheOneCall x >> return x

formatOutput :: Config -> OneCallRoot -> IO ()
formatOutput config oneCall = do
    let weatherIcon  = toWeatherCondition (current oneCall)
                     $ weather_id
                     $ case weatherList of
                           []    -> error "Empty response"
                           (x:_) -> x
                       where weatherList = C.weather $ current oneCall
    let temperature  = T (C.temp $ current oneCall) $ units config
    let rH           = C.humidity $ current oneCall
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
    putStr "%"
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

cacheOneCall :: (ToJSON a) => a -> IO ()
cacheOneCall o = do
    createDirectoryIfMissing True ".cache"
    encodeFile cachePath o
-- Pure Functions --

---- Constants
cachePath :: FilePath
cachePath = ".cache/cache.json"

defaultConfig :: Config
defaultConfig = Config { apiKey="Your API Key", loc="North Pole", units=Celsius }

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
oneCallRequest (inputLat, inputLon) unit = 
    setRequestPath "/data/3.0/onecall"
  $ setRequestQueryString [ ("lat"  , Just $ formatCoord inputLat)
                          , ("lon"  , Just $ formatCoord inputLon)
                          , ("units", Just $ formatUnits unit)
                          ]
    owmRequest

---- Miscellaneous Helpers
isDay :: Current -> Bool
isDay c = C.dt c >= C.sunrise c && C.dt c <= C.sunset c

cacheValid :: Maybe OneCallRoot -> POSIXTime -> POSIXTime -> Bool
cacheValid cache modT t = 
    case cache of
        Nothing -> False
        Just x  -> modT  <= C.dt (current x)
                && t     <= C.dt (current x) + 600
