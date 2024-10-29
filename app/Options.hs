{-# LANGUAGE DuplicateRecordFields #-}

module Options where

import Options.Applicative

data ForecastStyle = Basic | Complex

data Command = Now NowOptions 
             | FormatStr FormatStrOptions
             | Forecast  ForecastOptions
             | Calibrate

data Options = Options
    { optCommand :: Command }

data NowOptions = NowOptions
    { optStyle :: ForecastStyle }

data FormatStrOptions = FormatStrOptions
    { optFStr :: String }

data ForecastOptions = ForecastOptions
    { optDays  :: Int
    , optStyle :: ForecastStyle
--  , optHours :: Int
    }

----

pOptions :: Parser Options
pOptions = Options
    <$> subparser
    (  command "now" 
        ( info pNow
        $ progDesc "Output information about the current weather"
        )
    <> command "forecast"
        ( info pForecast
        $ progDesc "Output a weather forecast for a specified time period"
        )
    <> command "format"
        ( info pFormatStr
        $ progDesc "Output weather information according to a formatting string"
        )
    <> command "calibrate"
        ( info pCalibrate
        $ progDesc "Calibrate emoji widths"
        )
    )

---- Command Parsers ----

---- now ----
pNow :: Parser Command
pNow = Now <$> ( NowOptions <$> pStyle )

----
---- forecast ----
pForecast :: Parser Command
pForecast = Forecast <$> ( ForecastOptions <$> pDays <*> pStyle )

pDays :: Parser Int
pDays = option auto
    (  long    "days"
    <> short   'd'
    <> metavar "# OF DAYS"
    <> help    "Forecast N days"
    )
----
---- format ----
pFormatStr :: Parser Command
pFormatStr = FormatStr <$> ( FormatStrOptions <$> pFStr )

pFStr :: Parser String
pFStr = strOption
    (  long    "format"
    <> short   'f'
    <> metavar "FORMAT_STR" 
    <> help    "Output weather status to format specified by FORMAT_STR"
    )
----
---- calibrate ----
pCalibrate :: Parser Command
pCalibrate = pure Calibrate

---- Common ----
pStyle :: Parser ForecastStyle
pStyle = flag Basic Complex
    (  long  "complex" 
    <> short 'c'
    <> help  "Print a detailed forecast"
    )
----

-- hours :: Parser Int
-- hours = strOption
--     (  long    "hours"
--     <> short   "h"
--     <> metavar "INT"
--     <> help    "# of hours to forecast"
--     )

