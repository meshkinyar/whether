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

status :: Forecast -> T.Text
status = format (wc Symbolic <+> temp >+% rH Symbolic <+> humid <+> mp Symbolic)

dayComponent :: Component Forecast
dayComponent fp = cpadded (fromIntegral $ cellWidth fp) ' '
  $ datetime (dtStyle fp)

uviComponent :: Component Forecast
uviComponent fp = leftFixed fp
  $ uv (glyphStyle fp) %+> uvi (displayMode fp)

tempComponent :: Component Forecast
tempComponent fp = leftFixed fp
  $ te (glyphStyle fp) %+> temp

tempsComponent :: Component Forecast
tempsComponent fp = leftFixed fp
  $ te (glyphStyle fp) %+> tempL >+% "-" <+> tempH

humidityComponent :: Component Forecast
humidityComponent fp = leftFixed fp
  $ rH (glyphStyle fp) %+> humid

conditionComponent :: Component Forecast
conditionComponent fp = leftFixed fp
  $ wc (glyphStyle fp) <+> condition (displayMode fp)

windComponent :: Component Forecast
windComponent fp = leftFixed fp
  $ wd (glyphStyle fp) <+> wind (displayMode fp)

colPad :: FrameProperties -> Format r r
colPad fp = pad (displayMode fp)
  where
    pad Compact  = " "
    pad Expanded = "  "

leftFixed :: FrameProperties -> Format r (a -> r) -> Format r (a -> r)
leftFixed fp = lfixed (fromIntegral $ cellWidth fp) ' '
