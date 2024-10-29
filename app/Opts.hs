{-# LANGUAGE DuplicateRecordFields #-}

module Opts where

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
    { optFormatStr :: String }

data ForecastOptions = ForecastOptions
    { optDays  :: Int
    , optStyle :: ForecastStyle
--  , optHours :: Int
    }

-- opts :: Parser Options
-- opts = Options 
--     <$> 

-- optsParser :: Parser Command
-- optsParser = subparser
--     (  command "now" 
--         ( info pNow
--             ( progDesc "Output information about the current weather" )
--         )
--     <> command "forecast"
--         ( info commandForecast
--             ( progDesc "Output a weather forecast for a specified time period" )
--         )
--     <> command "format"
--         ( info commandFormat
--             ( progDesc "Output weather information according to a formatting string" )
--         )
--     )
-- 
-- pNow :: Parser NowOptions
-- pNow = NowOptions { optStyle = pStyle }

pStyle :: Parser ForecastStyle
pStyle = flag Basic Complex
    ( long   "complex" 
    <> short 'c'
    <> help  "Print a detailed forecast"
    )

pDays :: Parser Int
pDays = option auto
    ( long     "days"
    <> short   'd'
    <> metavar "N"
    <> help    "Forecast N days"
    )

pFormatStr :: Parser String
pFormatStr = strOption
    ( long     "format"
    <> short   'f'
    <> metavar "FORMAT_STR" 
    <> help    "Output weather status to format specified by FORMAT_STR"
    )

-- hours :: Parser Int
-- hours = strOption
--     (  long    "hours"
--     <> short   "h"
--     <> metavar "INT"
--     <> help    "# of hours to forecast"
--     )

