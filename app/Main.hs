{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Data.Aeson
import Network.HTTP.Simple
import Data.Ix                             ( inRange )
import Control.Monad                       ( unless )
import Data.Time.Clock.POSIX               ( getPOSIXTime, utcTimeToPOSIXSeconds, POSIXTime )
import Data.Text                           ( Text, pack )
import Data.Text.Encoding                  ( encodeUtf8 )
import Text.Read                           ( readMaybe )
import Text.Printf                         ( printf )
import Text.Casing                         ( pascal )
import Data.ByteString                     ( ByteString )
import System.Directory                    ( XdgDirectory( XdgConfig, XdgCache ), getXdgDirectory, createDirectoryIfMissing, doesFileExist, getModificationTime )
import System.FilePath                     ( takeDirectory )
import System.IO

import Types
import qualified Data.ByteString.Lazy as L ( ByteString )

-- Duplicate Fields --
import qualified Types as G                ( MatchedLocation(lat, lon) )
import qualified Types as R                ( OneCallRoot(lat, lon) )
import qualified Types as C                ( Current(dt, sunrise, sunset, temp, feels_like, pressure
                                           , humidity, dew_point, uvi, clouds, visibility
                                           , wind_speed, wind_deg, wind_gust, weather)
                                           )
import qualified Types as M                ( Minutely(dt) )
import qualified Types as H                ( Hourly(dt, temp, feels_like, pressure, humidity, dew_point
                                           , uvi, clouds, visibility, wind_speed, wind_gust, weather, pop)
                                           )  
import qualified Types as D                ( Daily(dt, sunrise, sunset, pressure, dew_point, humidity
                                           , wind_speed, wind_deg, wind_gust, weather, clouds, pop, uvi)
                                           )


main :: IO ()
main = do
    config  <- getConfig 
    oneCall <- getOneCall config
    formatOutput config oneCall

getConfig :: IO Config
getConfig = do 
    configPath      <- getConfigPath :: IO FilePath
    configExists    <- doesFileExist configPath
    unless configExists $ createConfig configPath
    configFile       <- decodeFileStrict configPath :: IO (Maybe ConfigRoot)
    lastModification <- getModificationTime configPath
    case configFile of
        Nothing    -> error "Invalid config.json"
        Just root  -> return (root, lastModification)  

getOneCall :: Config -> IO OneCallRoot
getOneCall (cfg, t) = do
    cachePath <- getCachePath
    cacheFile <- decodeFileStrictIfExists cachePath :: IO (Maybe OneCallRoot)
    lastCache <- cacheIfValid cacheFile (utcTimeToPOSIXSeconds t) <$> getPOSIXTime
    case lastCache of
        Just x  -> return x
        Nothing -> do
            -- TODO: separate geocoding cache
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
                Left  x   -> print oneCallResponse >> error x
                Right x   -> cacheOneCall x >> return x

formatOutput :: Config -> OneCallRoot -> IO ()
formatOutput (config, _) oneCall = do
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
        then decodeFileStrict path
        else return Nothing

cacheOneCall :: (ToJSON a) => a -> IO ()
cacheOneCall onecall = do
    cachePath <- getCachePath
    createDirectoryIfMissing True $ takeDirectory cachePath
    encodeFile cachePath onecall

createConfig :: FilePath -> IO ()
createConfig path = do
    hSetBuffering stdout NoBuffering
    putStrLn  $ "No config.json found, creating new config.json at" ++ path
    putStr   "Please enter the name of your location: "
    newLoc    <- getLine
    putStr   "Please enter your API Key: "
    newApiKey <- getLine
    newUnits  <- validateInput Celsius "Please choose a unit system"
    createDirectoryIfMissing True $ takeDirectory path
    encodeFile path $ newConfig (newApiKey, newLoc, newUnits)

-- IO Helpers --

getConfigPath :: IO FilePath
getConfigPath = getXdgDirectory XdgConfig "whether/config.json"

getCachePath :: IO FilePath
getCachePath = getXdgDirectory XdgCache "whether/cache.json"

validateInput :: forall a . (Show a, Read a) => a -> String -> IO String
validateInput _default prompt = do
    putStr $ printf "%s [default: %s]: " prompt $ show _default
    input <- getLine
    if input == ""
        then return $ show _default
        else case readMaybe (pascal input) :: Maybe a of
            Nothing -> validateInput _default "Invalid input. Choose one of"
            Just _  -> return input


-- Pure Functions --

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
formatUnits x = encodeUtf8 $ pack $ oneCallUnits x

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

oneCallUnits :: TemperatureUnit -> String
oneCallUnits u = case u of
   Kelvin    -> "standard"
   Celsius   -> "metric"
   Farenheit -> "imperial"


newConfig :: (String, String, String) -> ConfigRoot
newConfig (k, l, u) =
    ConfigRoot { apiKey = pack k
               , loc    = pack l
               , units  = read u :: TemperatureUnit
               }

cacheIfValid :: Maybe OneCallRoot -> POSIXTime -> POSIXTime -> Maybe OneCallRoot
cacheIfValid cache modT t = 
    case cache of
        Nothing -> Nothing
        Just x  -> case isValid of 
            False -> Nothing
            True  -> Just x
          where
            isValid = modT <= C.dt (current x)
                   && t    <= C.dt (current x) + 600
