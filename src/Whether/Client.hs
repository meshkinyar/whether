{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Whether.Client where

import Control.Monad                       ( when, unless )
import Data.Aeson                          ( ToJSON, FromJSON, encodeFile, decodeFileStrict )
import Data.Time.Clock.POSIX
import Data.Time.Zones
import Formatting                          ( (%), formatToString, string )
import GHC.Generics
import Optics
import System.Directory                    ( XdgDirectory( XdgConfig, XdgCache, XdgState ), createDirectoryIfMissing, doesFileExist, getModificationTime, getXdgDirectory )
import System.FilePath                     ( takeDirectory, (</>) )
import System.Exit                         ( die )
import System.IO                           ( hSetBuffering, stdout, BufferMode(NoBuffering) )
import Text.Casing                         ( pascal )
import Text.Read                           ( readMaybe )
import Toml
import Toml.Pretty
import Toml.Schema
import qualified Data.Text as S            ( Text, pack, show, concat )
import qualified Data.Text.IO as S         ( readFile, writeFile, getLine, putStr )
import qualified Data.ByteString.Lazy as L ( ByteString )
 
import Whether.Display
import Whether.Config as Config
import Whether.Display.Frame
import Whether.Units

instance FromJSON UnitSystem
instance ToJSON UnitSystem

-- | Represents a supported weather API provider.
data WeatherAPI = OpenWeatherMap
  deriving (Eq, Read, Show, Generic)

-- | Gets a @Config@ value from a config file, adding default values where necessary.
getConfig :: IO Config
getConfig = do
  systemTZ <- loadLocalTZ
  toConfig systemTZ <$> getConfigFile

-- | Retrieves a @ConfigFile@ record from the config file on disk.
getConfigFile :: IO ConfigFile
getConfigFile = do
  configPath   <- getXdgDirectory XdgConfig "whether/config.toml"
  configExists <- doesFileExist configPath
  unless configExists $ createNewConfigFile configPath
  configFile   <- S.readFile configPath
  case Toml.decode configFile of
    Failure es      -> die $ foldl (<>) "Invalid config.toml: " es
    Success [] cfg  -> return cfg 
    Success es cfg  -> putStrLn (foldl (<>) "Warning when decoding config file:" es)
                    >> return cfg

-- | Interactively creates a new config if one doesn't already exist.
createNewConfigFile :: FilePath -> IO ()
createNewConfigFile path = do
  hSetBuffering stdout NoBuffering
  putStrLn  $ "No config.toml found, creating new config.toml at " <> path
  api        <- validateInputValue OpenWeatherMap
              "Please choose a weather API provider: "
  putStr      "Please enter your API Key: "
  key        <- S.getLine
  putStr      "Please enter the name of your location: "
  loc        <- S.getLine
  units      <- validateInputValue Metric
              "Please choose a unit system "
  tn         <- validateInputValue TwentyFourHour
              "Please choose the time notation "
  ls         <- validateInputValue Rounded
              "Please choose a line style "
  createDirectoryIfMissing True $ takeDirectory path
  S.writeFile path . S.show . Toml.encode $ mkOWMConfigFile (api, key, loc, units, tn, ls)

-- | Creates a new config from the provided values.
mkOWMConfigFile :: (WeatherAPI, S.Text, S.Text, UnitSystem, TimeNotation, LineStyle) -> ConfigFile
mkOWMConfigFile (api, key, loc, units, tn, ls) =
  ConfigFile
    {
      location       = loc
    , unitSystem     = units
    , timeNotation   = tn
    , lineStyle      = ls
    , glyphStyle     = Nothing -- Optional override
    , dayStyle       = Nothing
    , currentStyle   = Nothing
    , openWeatherMap = owm api
    , timezone       = Nothing
    }
    where
      owm OpenWeatherMap = Just OWMConfig { apiKey = key }
      -- owm _ = Nothing

-- | Checks when the config file was last modified.
getConfigLastMod :: IO POSIXTime
getConfigLastMod = do
  configPath <- getXdgDirectory XdgConfig "whether/config.toml"
  modT       <- utcTimeToPOSIXSeconds <$> getModificationTime configPath
  return $ realToFrac modT

-- | Validate that the input can be read to type a.
validateInputValue :: forall a . (Show a, Read a) => a -> S.Text -> IO a
validateInputValue defaultOpt basePrompt = do
  S.putStr $ S.concat [basePrompt, "[default: ", S.show defaultOpt, "]: "]
  input <- getLine
  if input == ""
  then return defaultOpt
  else case readMaybe (pascal input) of
    Nothing -> validateInputValue defaultOpt "Invalid input. Choose one of"
    Just i  -> return i

-- | Gets whether the cache file is stil valid.
-- If the user has modified their config before the last response was cached or
-- the cache file was saved > 10 minutes ago, then the cache is no long valid.
-- TODO: make cache invalidation time configurable.
isCacheValid :: POSIXTime -> POSIXTime -> POSIXTime -> Bool
isCacheValid cacheTime configLastMod now = configLastMod <= cacheTime && now <= cacheTime + 600

-- | Saves an API response to a JSON cache file.
-- Stores the cache file in the system's standard XDG cache directory.
cacheJSON :: (ToJSON a) => FilePath -> a -> IO ()
cacheJSON filename obj = do
  cachePath <- getXdgDirectory XdgCache "whether"
  createDirectoryIfMissing True $ takeDirectory cachePath
  encodeFile (cachePath </> filename) obj

-- | Gets a record from a JSOM cache file.
getJSONCache :: (FromJSON a) => FilePath -> IO (Maybe a)
getJSONCache filename = do
  cachePath <- getXdgDirectory XdgCache "whether"
  let filePath = cachePath </> filename
  exists    <- doesFileExist filePath
  if exists
  then decodeFileStrict filePath
  else return Nothing

-- | The number of seconds that a lock file prevents execution
lockDuration :: POSIXTime
lockDuration = 300

-- | Gets when the last created lock file was modified.
getLockTime :: FilePath -> IO POSIXTime
getLockTime lockPath = do
  lockExists <- doesFileExist lockPath
  if lockExists
  then do
    t <- utcTimeToPOSIXSeconds <$> getModificationTime lockPath
    return $ realToFrac t
  else return 0

-- | Creates an empty lock file that prevents execution of whether.
setLock :: IO ()
setLock = do 
  lockPath <- getXdgDirectory XdgState "whether"
  createDirectoryIfMissing True lockPath
  S.writeFile ( lockPath </> "lock" ) ""
