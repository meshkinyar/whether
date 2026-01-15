{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Data.Text.IO as T         ( putStr, putStrLn )

import Whether.OWM.Types
import Whether.OWM.Client
import Whether.Client
import Whether.Config
import Whether.Components

import Options
import Options.Applicative

import qualified Options as Forecast       ( ForecastOptions(optStyle) )

main :: IO ()
main = do
  opts   <- execParser (info pOptions idm)
  config <- getConfig OpenWeatherMap
  case optCommand opts of
    Now      o -> cmdNow       config o
    Forecast o -> cmdForecast  config o
    Calibrate  -> cmdCalibrate

-- Print information about the current weather
cmdNow :: Config -> NowOptions -> IO ()
cmdNow config _ = do
    oneCall <- getOneCall True config
    T.putStr (status $ getCurrentWeather config oneCall)

-- Print a forecast
cmdForecast :: Config -> ForecastOptions -> IO ()
cmdForecast config opt = do
    oneCall <- getOneCall True config
    T.putStrLn $ undefined frame (optDays opt) $ getDailyForecasts config oneCall
      where
        frame = case Forecast.optStyle opt of
          BasicOption    -> undefined
          ExpandedOption -> undefined

-- Calibrate Emoji widths
cmdCalibrate :: IO ()
cmdCalibrate = putStrLn "WIP"
