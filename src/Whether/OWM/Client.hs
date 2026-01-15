{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Whether.OWM.Client where

import Data.Aeson                          ( eitherDecode, decode )
import Control.Monad                       ( when                 )
import Data.ByteString                     ( ByteString           )
import Data.Text.Encoding                  ( encodeUtf8           )
import Data.Time.Clock.POSIX
import GHC.Generics
import Network.HTTP.Simple
import Optics
import System.FilePath                     ( takeDirectory, (</>) )
import System.Directory                    ( XdgDirectory( XdgConfig, XdgCache, XdgState ), getXdgDirectory, createDirectoryIfMissing, doesFileExist )
import System.Exit                         ( die )

import qualified Data.Text as S            ( Text, pack, null     )
import qualified Data.ByteString.Lazy as L ( ByteString )

import Whether.Client
import Whether.Units
import Whether.Config
import Whether.OWM.Types

ensureOWMAPIKey :: Config -> Either String S.Text
ensureOWMAPIKey cfg = ensure (openWeatherMap cfg)
  where
    ensure Nothing = Left "Missing entry for 'config.openWeatherMap' in config.toml."
    ensure (Just owm)
      | S.null key = Left "The apiKey entry for 'openWeatherMap' is empty."
      | otherwise  = Right key
      where
        key = apiKey owm

-- Call the OWM API
callOWMAPI :: Either String S.Text -> Request -> IO L.ByteString
callOWMAPI (Left msg) _ = die msg
callOWMAPI (Right key) request = do
  let r = addToRequestQueryString [("appid", Just $ encodeUtf8 key)] request
  response <- httpLBS r
  return ( getResponseBody response )

-- Simple HTTP Requests
owmRequest :: Request
owmRequest =
    setRequestMethod "GET"
  $ setRequestSecure True
  $ setRequestPort 443
  $ setRequestHost "api.openweathermap.org"
    defaultRequest

-- | Request type for geocoding API requests
geocodeRequest :: S.Text -> Request
geocodeRequest location =
    setRequestPath "/geo/1.0/direct"
  $ setRequestQueryString [ ("q", Just $ encodeUtf8 location)
                          , ("limit", Just "1")
                          ]
    owmRequest

-- | Request type for OneCall 3.x requests
oneCallRequest :: Coordinates -> UnitSystem -> Request
oneCallRequest (inputLat, inputLon) unit =
    setRequestPath "/data/3.0/onecall"
  $ setRequestQueryString [ ("lat"  , Just $ formatCoord inputLat)
                          , ("lon"  , Just $ formatCoord inputLon)
                          , ("units", Just $ formatTUnits unit)
                          ]
    owmRequest
  where
    formatCoord = encodeUtf8 . S.pack . show

formatTUnits :: UnitSystem -> ByteString
formatTUnits x = encodeUtf8 $ S.pack $ show x

-- Retrieve from cache if valid or call the OWM OneCall API for a fresh JSON
getOneCall :: Bool -> Config -> IO OneCallRoot
getOneCall lockOnFail cfg = do
  now      <- getPOSIXTime
  lockPath <- getXdgDirectory XdgState "whether/lock"
  lockT    <- getLockTime lockPath

  -- Stop execution if last response could not be parsed
  -- Prevents API calls from being exhausted by bugs
  when (lockOnFail && now < realToFrac lockT + 300) $
    die $ "New API calls locked due to failed parse, delete " <> lockPath <> " to retry."

  cacheFile <- getJSONCache "oneCall.json" :: IO (Maybe OneCallRoot)
  lastMod   <- getConfigLastMod
  case cacheFile of
    Just cache | isCacheValid (cache ^. #current % #dt) lastMod now
                           -> return cache
               | otherwise -> getNew
    Nothing                -> getNew
    where
      getNew = do
        location    <- getLocation cfg
        oneCallResp <- callOWMAPI (ensureOWMAPIKey cfg)
                     $ oneCallRequest (location ^. #lat, location ^. #lon)
                     $ unitSystem cfg
        case (eitherDecode oneCallResp :: Either String OneCallRoot) of
          Left x  -> do setLock
                        die x
          Right x -> cacheJSON "oneCall.json" x
                  >> cacheJSON "unitSystem.json" (unitSystem cfg)
                  >> return x

-- Get the first matched location from the OWM geocoding API
getLocation :: Config -> IO MatchedLocation
getLocation cfg = do
  geocodeCache <- getJSONCache "geocode.json" :: IO (Maybe GeocodeRoot)
  case geocodeCache of
    Just (match:_) ->
      if name match == location cfg
        then return match
        else getGeocode
    _              -> getGeocode
    where
    getGeocode = do
      geocodeResp <- callOWMAPI (ensureOWMAPIKey cfg)
                  $  geocodeRequest
                  $  location cfg
      case (decode geocodeResp :: Maybe GeocodeRoot) of
        Nothing    -> die "Invalid Geocoding Response. Ensure that your API key in config.json is valid."
        Just []    -> die "Empty Geocoding Response."
        Just (g:_) -> cacheJSON "geocode.json" g >> return g

