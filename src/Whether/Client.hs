{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Whether.Client where

import Control.Monad            ( when, unless )
import Data.Aeson               ( ToJSON, FromJSON, encodeFile, decodeFileStrict )
import Data.Time.Clock.POSIX
import Formatting               ( (%), formatToString, string )
import GHC.Generics
import System.Directory         ( XdgDirectory( XdgConfig, XdgCache, XdgState ), createDirectoryIfMissing, doesFileExist, getModificationTime, getXdgDirectory )
import System.FilePath          ( takeDirectory, (</>) )
import System.Exit              ( die )
import System.IO                ( hSetBuffering, stdout, BufferMode(NoBuffering) )
import Text.Read                ( readMaybe )
import Toml
import Toml.Pretty
import Toml.Schema

import Text.Casing              ( pascal )
import qualified Data.Text as S ( Text, pack, show, concat )
import qualified Data.Text.IO as S ( readFile, writeFile, getLine, putStr )
import qualified Data.ByteString.Lazy as L ( ByteString )
 
import Whether.Config
import Whether.Frame
import Whether.Units

instance FromJSON UnitSystem
instance ToJSON UnitSystem

data WeatherAPI = OpenWeatherMap

-- | Retrieve the whether config file for the given API
getConfig :: WeatherAPI -> IO Config
getConfig OpenWeatherMap = do
    configPath   <- getXdgDirectory XdgConfig "whether/config.toml"
    configExists <- doesFileExist configPath
    unless configExists $ createConfig configPath
    configFile   <- S.readFile configPath
    case Toml.decode configFile of
      Failure es      -> die $ foldl (<>) "Invalid config.toml: " es
      Success [] cfg  -> return cfg 
      Success es cfg  -> putStrLn (foldl (<>) "Warning when decoding config file:" es)
                      >> return cfg

-- | Mini CLI to create a config if it doesn't already exist
createConfig :: FilePath -> IO ()
createConfig path = do
  hSetBuffering stdout NoBuffering
  putStrLn  $ "No config.toml found, creating new config.toml at " <> path
  putStr      "Please enter the name of your location: "
  newLoc        <- S.getLine
  putStr      "Please enter your API Key: "
  newApiKey     <- S.getLine
  newUnits      <- validateInput Metric
              "Please choose a unit system "
  newLineStyle  <- validateInput Rounded
              "Please choose a line style "
  createDirectoryIfMissing True $ takeDirectory path
  S.writeFile path . S.show . Toml.encode $ mkOWMConfig (newLoc, newUnits, newLineStyle, newApiKey)

-- | Create a 
mkOWMConfig :: (S.Text, UnitSystem, LineStyle, S.Text) -> Config
mkOWMConfig (l, u, ls, k) = Config
  {
    location                 = l
  , unitSystem               = u
  , Whether.Config.lineStyle = ls
  , openWeatherMap           = Just OWMConfig { apiKey = k }
  }

-- Check when the config file was last modified
getConfigLastMod :: IO POSIXTime
getConfigLastMod = do
  configPath <- getXdgDirectory XdgConfig "whether/config.toml"
  modT       <- utcTimeToPOSIXSeconds <$> getModificationTime configPath
  return $ realToFrac modT

-- Validate that the input can be represented as the same type as _default
validateInput :: forall a . (Show a, Read a) => a -> S.Text -> IO a
validateInput _default basePrompt = do
  S.putStr $ S.concat [basePrompt, "[default: ", S.show _default, "]: "]
  input <- getLine
  if input == ""
    then return _default
    else case readMaybe (pascal input) of
      Nothing -> validateInput _default "Invalid input. Choose one of"
      Just i  -> return i

isCacheValid :: POSIXTime -> POSIXTime -> POSIXTime -> Bool
isCacheValid cacheTime configLastMod now = configLastMod <= cacheTime && now <= cacheTime + 600

-- Check when the last created lock file was modified
getLockTime :: FilePath -> IO POSIXTime
getLockTime lockPath = do
  lockExists <- doesFileExist lockPath
  if lockExists
  then do
    t <- utcTimeToPOSIXSeconds <$> getModificationTime lockPath
    return $ realToFrac t
  else return 0

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
  let filePath = cachePath </> filename
  exists    <- doesFileExist filePath
  if exists
  then decodeFileStrict filePath
  else return Nothing

-- Create an empty lock file to prevent exhaustion of API calls 
setLock :: IO ()
setLock = do 
    lockPath <- getXdgDirectory XdgState "whether"
    createDirectoryIfMissing True lockPath
    S.writeFile ( lockPath </> "lock" ) ""
