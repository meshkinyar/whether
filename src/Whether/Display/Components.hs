{-# LANGUAGE OverloadedStrings #-}

module Whether.Display.Components where

import Data.Int
import Formatting

import Whether.Weather
import Whether.Display
import Whether.Display.Formatters
import Whether.Display.Frame

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T

-- 
tmuxStatus :: Forecast -> T.Text
tmuxStatus = format (wc Symbolic <+> temp >+% rH Symbolic <+> humid <+> mp Symbolic)

-- | Formatting logic provider for centered timestamps.
dayComponent :: Component Forecast
dayComponent fp = cpadded (fromIntegral $ cellWidth fp) ' '
  $ datetime (dtStyle fp)

-- | Formatting logic provider for UVI.
-- Adds a static flat sun icon next to a UV description.
uviComponent :: Component Forecast
uviComponent fp = leftFixed fp
  $ uv (glyphStyle fp) %+> uvi (displayMode fp)

-- | Formatting logic provider for temperature.
-- Adds a temperature glyph next to a temperature reading.
tempComponent :: Component Forecast
tempComponent fp = leftFixed fp
  $ te (glyphStyle fp) %+> temp

-- | Formatting logic provider for high and low temperature.
-- Adds a temperature glyph next to the temperature range.
tempsComponent :: Component Forecast
tempsComponent fp = leftFixed fp
  $ te (glyphStyle fp) %+> tempL >+% "-" <+> tempH

-- | Formatting logic provider for humidity.
-- Adds a humidity glyph next to the relative humidity percentage.
humidityComponent :: Component Forecast
humidityComponent fp = leftFixed fp
  $ rH (glyphStyle fp) %+> humid

-- | Formatting logic provider for weather condition.
-- Adds a glyph of the current weather condition next to a description.
conditionComponent :: Component Forecast
conditionComponent fp = leftFixed fp
  $ wc (glyphStyle fp) <+> condition (displayMode fp)

-- | Formatting logic provider for wind.
-- Adds a glyph showing the direction of the wind next to the speed.
windComponent :: Component Forecast
windComponent fp = leftFixed fp
  $ wd (glyphStyle fp) <+> wind (displayMode fp)

-- | Helper function that determines the @lfixed@ width from @FrameProperties@.
leftFixed :: FrameProperties -> Format r (a -> r) -> Format r (a -> r)
leftFixed fp = lfixed (fromIntegral $ cellWidth fp) ' '
