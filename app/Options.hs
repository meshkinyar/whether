{-# LANGUAGE DuplicateRecordFields #-}

module Options where

import Options.Applicative
import Data.Ix ( inRange )

data ForecastMode = ExpandedOption | CompactOption 

data Command = Forecast   ForecastOptions
             | TmuxStatus TmuxStatusOptions 
             | Calibrate
             
newtype Options = Options
    { optCommand :: Command }

data TmuxStatusOptions = TmuxStatusOptions
    { optStyle  :: ForecastMode
    , optFormat :: Maybe String
    , optLoc    :: Maybe String
    }

data ForecastOptions = ForecastOptions
    { optDays  :: Int
    , optStyle :: ForecastMode
    , optLoc   :: Maybe String
--  , optHours :: Int
    }

----

pOptions :: Parser Options
pOptions = Options
    <$> subparser
    (  command "tmuxStatus" 
        ( info pTmuxStatus
        $ progDesc "Output information about the current weather"
        )
    <> command "forecast"
        ( info pForecast
        $ progDesc "Output a weather forecast for a specified time period"
        )
    <> command "calibrate"
        ( info pCalibrate
        $ progDesc "Calibrate emoji widths"
        )
    )

---- Command Parsers ----

---- forecast ----
pForecast :: Parser Command
pForecast = Forecast <$> ( ForecastOptions <$> pDays <*> pMode <*> pLoc )

days :: ReadM Int
days = do
    n <- auto
    case inRange (1, 8) n of
        True  -> return n
        False -> readerError "Days must be in the range 1-8"

pDays :: Parser Int
pDays = option days
    (  long    "days"
    <> short   'd'
    <> metavar "# DAYS (1-8)"
    <> help    "Forecast N days"
    )
----

---- tmuxStatus ----
pTmuxStatus :: Parser Command
pTmuxStatus = TmuxStatus <$> ( TmuxStatusOptions <$> pMode <*> pFormat <*> pLoc )
----

---- calibrate ----
pCalibrate :: Parser Command
pCalibrate = pure Calibrate

---- Common ----
pMode :: Parser ForecastMode
pMode = flag ExpandedOption CompactOption
    (  long  "compact" 
    <> short 'c'
    <> help  "Print a compact forecast"
    )

pLoc :: Parser (Maybe String)
pLoc = optional $ strOption
    (  long    "location"
    <> short   'l'
    <> metavar "LOCATION"
    <> help    "Get weather information for matched LOCATION"
    )

pFormat :: Parser (Maybe String)
pFormat = optional $ strOption
    (  long    "format"
    <> short   'f'
    <> metavar "FORMAT_STR" 
    <> help    "Output weather status to format specified by FORMAT_STR"
    )

----

-- hours :: Parser Int
-- hours = strOption
--     (  long    "hours"
--     <> short   "h"
--     <> metavar "INT"
--     <> help    "# of hours to forecast"
--     )

