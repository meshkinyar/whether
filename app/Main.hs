{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Data.Aeson
import Network.HTTP.Simple
import Control.Monad                       ( when, unless )
import Data.Time.Clock.POSIX               
import Data.Text                           ( Text, pack )
import Data.Text.Encoding                  ( encodeUtf8 )
import Text.Read                           ( readMaybe )
import Formatting
import Text.Casing                         ( pascal )
import System.Posix.Files                  ( getFileStatus, modificationTime )
import System.Directory                    ( XdgDirectory( XdgConfig, XdgCache, XdgState ), getXdgDirectory, createDirectoryIfMissing, doesFileExist )
import System.FilePath                     ( takeDirectory, (</>) )
import System.IO

import Types
import Conversions
import Display
import Options
import Options.Applicative
import qualified Data.Text.IO as T         ( putStr, putStrLn )
import qualified Data.ByteString.Lazy as L ( ByteString )

-- Duplicate Field handling --
-- import qualified Types as R                ( OneCallRoot(lat, lon) )
import qualified Types as G                ( MatchedLocation(lat, lon) )
import qualified Types as C                ( Current(dt) )
-- import qualified Types as C                ( Current(dt, sunrise, sunset, temp, feels_like, pressure
--                                            , humidity, dew_point, uvi, clouds, visibility
--                                            , wind_speed, wind_deg, wind_gust, weather)
--                                            )
-- import qualified Types as M                ( Minutely(dt) )
-- import qualified Types as H                ( Hourly(dt, temp, feels_like, pressure, humidity, dew_point
--                                            , uvi, clouds, visibility, wind_speed, wind_gust, weather, pop)
--                                            )  
-- import qualified Types as D                ( Daily(dt, sunrise, sunset, pressure, dew_point, humidity
--                                            , wind_speed, wind_deg, wind_gust, weather, clouds, pop, uvi)
--                                            )


main :: IO ()
main = do
    config <- getConfig 
    opts   <- execParser (info pOptions idm)
    case (optCommand opts) of
        Now       o -> cmdNow       config o
        Forecast  o -> cmdForecast  config o
        FormatStr o -> cmdFormatStr config o
        Calibrate   -> cmdCalibrate

-- Print information about the current weather
cmdNow :: Config -> NowOptions -> IO ()
cmdNow config _ = do
    oneCall <- getOneCall True config
    T.putStr (statusString $ getCurrentWeather config oneCall)

-- Print a forecast
cmdForecast :: Config -> ForecastOptions -> IO ()
cmdForecast config opt = do
    oneCall <- getOneCall True config
    T.putStrLn (basicForecast (optDays opt) $ getDailyForecast config oneCall) 

-- Print a custom formatted string
cmdFormatStr :: Config -> FormatStrOptions -> IO ()
cmdFormatStr config _ = do
    _ <- getOneCall True config
    T.putStrLn "WIP"

-- Calibrate Emoji widths
cmdCalibrate :: IO ()
cmdCalibrate = putStrLn "WIP"

-- Retrieve values from the whether config file
getConfig :: IO Config
getConfig = do
    configPath   <- getXdgDirectory XdgConfig "whether/config.json"
    configExists <- doesFileExist configPath
    unless configExists $ createConfig configPath
    configFile   <- decodeFileStrict configPath :: IO (Maybe Config)
    case configFile of
        Nothing   -> error "Invalid config.json"
        Just cfg  -> return cfg 

-- Retrieve from cache if valid or call the OWM OneCall API for a fresh JSON
getOneCall :: Bool -> Config -> IO OneCallRoot
getOneCall lockOnFail cfg = do
    time0         <- getPOSIXTime
    lockPath      <- getXdgDirectory XdgState "whether/lock"
    lockT         <- getLockTime lockPath

    -- Stop execution if last response could not be parsed
    -- Prevents API calls from being exhausted by bugs
    when (lockOnFail && time0 < realToFrac lockT + 300) $
        error $ formatToString ("New API calls locked due to failed parse, delete " % string % " to retry.") lockPath

    cacheFile     <- getJSONCache "oneCall.json" :: IO (Maybe OneCallRoot)
    lastMod       <- getConfigLastMod
    let isValid cache = lastMod <= C.dt (current cache)
                     && time0   <= C.dt (current cache) + 600
    case cacheFile of
        Just x | isValid x -> return x
               | otherwise -> getNew
        Nothing            -> getNew
        where
          getNew = do
              location    <- getLocation cfg
              oneCallResp <- callAPI (apiKey cfg)
                           $ oneCallRequest (G.lat location, G.lon location)
                           $ unitSystem cfg
              case (eitherDecode oneCallResp :: Either String OneCallRoot) of
                  Left x  -> do setLock
                                error x
                  Right x -> cacheJSON "oneCall.json" x >> return x

-- Check when the last created lock file was modified
getLockTime :: FilePath -> IO POSIXTime
getLockTime lockPath = do
    lockExists <- doesFileExist lockPath
    if lockExists
    then do
        t <- modificationTime <$> getFileStatus lockPath
        return $ realToFrac t
    else return 0

-- Get the first matched location from the OWM geocoding API
getLocation :: Config -> IO (MatchedLocation)
getLocation cfg = do
    geocodeCache <- getJSONCache "geocode.json" :: IO (Maybe GeocodeRoot)
    case geocodeCache of
        Just (match:_) -> 
            if (name match == loc cfg) 
              then return match
              else getGeocode 
        _              -> getGeocode
      where
        getGeocode = do
            geocodeResp <- callAPI (apiKey cfg) 
                        $  geocodeRequest
                        $  loc cfg
            case (decode geocodeResp :: Maybe GeocodeRoot) of
                Nothing    -> error "Invalid Geocoding Response. Ensure that your API key in config.json is valid."
                Just []    -> error "Empty Geocoding Response."
                Just (g:_) -> cacheJSON "geocode.json" g >> return g


-- Call an OWM API
callAPI :: Text -> Request -> IO L.ByteString
callAPI key requestName = do
    let r = addToRequestQueryString [("appid", Just $ encodeUtf8 key)] requestName
    response <- httpLBS r
    return ( getResponseBody response )

-- Save an API response to a JSON cache file
-- Uses standard POSIX directory for cache
cacheJSON :: (ToJSON a) => FilePath -> a -> IO ()
cacheJSON filename obj = do
    cachePath <- getXdgDirectory XdgCache "whether"
    createDirectoryIfMissing True $ takeDirectory cachePath
    encodeFile (cachePath </> filename) obj

-- Get cached JSON from filename
getJSONCache :: (FromJSON a) => FilePath -> IO (Maybe a)
getJSONCache filename = do
    cachePath <- getXdgDirectory XdgCache "whether"
    exists    <- doesFileExist cachePath
    if exists
    then decodeFileStrict $ cachePath </> filename
    else return Nothing

-- CLI to create a config if it doesn't already exist
createConfig :: FilePath -> IO ()
createConfig path = do
    hSetBuffering stdout NoBuffering
    putStrLn  $ "No config.json found, creating new config.json at" ++ path
    putStr      "Please enter the name of your location: "
    newLoc    <- getLine
    putStr      "Please enter your API Key: "
    newApiKey <- getLine
    newUnits <- validateInput Metric
                "Please choose a unit system"
    createDirectoryIfMissing True $ takeDirectory path
    encodeFile path $ newConfig (newApiKey, newLoc, newUnits)

-- IO Helpers --

-- Check when the config file was last modified
getConfigLastMod :: IO POSIXTime
getConfigLastMod = do
    configPath <- getXdgDirectory XdgConfig "whether/config.json"
    modT       <- modificationTime <$> getFileStatus configPath
    return $ realToFrac modT

-- Validate that the input can be represented as the same type as _default
validateInput :: forall a . (Show a, Read a) => a -> String -> IO String
validateInput _default basePrompt = do
    putStr $ formatToString promptF basePrompt (show _default)
    input <- getLine
    if input == ""
        then return $ show _default
        else case readMaybe (pascal input) :: Maybe a of
            Nothing -> validateInput _default "Invalid input. Choose one of"
            Just _  -> return input
      where
        promptF = string % "[default: " % string % "]: "

-- Create an empty lock file to prevent exhaustion of API calls 
setLock :: IO ()
setLock = do 
    lockPath <- getXdgDirectory XdgState "whether"
    createDirectoryIfMissing True lockPath
    writeFile ( lockPath </> "lock" ) ""

-- Pure Functions --

---- Simple HTTP Requests
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

oneCallRequest :: Coordinates -> UnitSystem -> Request
oneCallRequest (inputLat, inputLon) unit = 
    setRequestPath "/data/3.0/onecall"
  $ setRequestQueryString [ ("lat"  , Just $ formatCoord inputLat)
                          , ("lon"  , Just $ formatCoord inputLon)
                          , ("units", Just $ formatTUnits unit)
                          ]
    owmRequest

---- Miscellaneous Helpers ----

-- Config constructor function
newConfig :: (String, String, String) -> Config
newConfig (k, l, u) =
    Config { apiKey     = pack k
           , loc        = pack l
           , unitSystem = read u :: UnitSystem
           }
