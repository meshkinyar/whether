{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Main where

import qualified Data.Text.Lazy.Builder as TB ( toLazyText )
import qualified Data.Text.Lazy.IO as T       ( putStr, putStrLn )

import Whether.OWM.Types
import Whether.OWM.Client
import Whether.Client
import Whether.Config
import Whether.Display.Components
import Whether.Display.Frame
import Whether.Display.Forecasts

import Options
import Options.Applicative

main :: IO ()
main = do
  opts   <- execParser (info pOptions idm)
  config <- getConfig
  case optCommand opts of
    Forecast   o -> cmdForecast   config o
    TmuxStatus o -> cmdTmuxStatus config o
    Calibrate    -> cmdCalibrate

-- Print information about the current weather
cmdTmuxStatus :: Config -> TmuxStatusOptions -> IO ()
cmdTmuxStatus config _ = do
  oneCall <- getOneCall True config
  T.putStr (tmuxStatus $ getCurrentWeather config oneCall)

-- Print a forecast
cmdForecast :: Config -> ForecastOptions -> IO ()
cmdForecast config ForecastOptions{optStyle, optDays} = do
  oneCall <- getOneCall True config
  T.putStrLn . TB.toLazyText . frame . take optDays
    $ getDailyForecasts config oneCall
    where
      frame = case optStyle of
        CompactOption  -> formatFrame (detailFrame config) -- TODO: Add compactFrame
        ExpandedOption -> formatFrame (detailFrame config)

-- Calibrate Emoji widths
cmdCalibrate :: IO ()
cmdCalibrate = putStrLn "WIP"
