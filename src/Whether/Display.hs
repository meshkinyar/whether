{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExistentialQuantification #-}

module Whether.Display where

import Data.Time.Format
import Data.Time.Clock.POSIX    ( posixSecondsToUTCTime )
import Data.Time.Clock          ( UTCTime )
import Formatting
import Formatting.Time

import qualified Data.Text as S ( Text, pack, unpack, show, intercalate, replicate, length )

import Whether.Weather
import Whether.Units 

newtype DTStyle = DayStyle DayStyle

data DayStyle = DayAbbr | DateDash

data DisplayStyle = Compact | Expanded

data StaticIcon = Thermometer
                | DropletWide
                | SunFlat
                | TriangleUp
                | TriangleDown
                | TriangleUpSmall
                | TriangleDownSmall

data ContentStyle = Textual | Symbolic

class Display a where
  display :: DisplayStyle -> a -> S.Text

class Symbol a where
  symbol :: a -> S.Text

instance Display MoonPhase where
  display Compact NewMoon        = "NM"
  display Compact WaxingCrescent = "WXC"
  display Compact FirstQuarter   = "FQ"
  display Compact WaxingGibbous  = "WXG"
  display Compact FullMoon       = "FM"
  display Compact WaningGibbous  = "WNG"
  display Compact LastQuarter    = "LQ"
  display Compact WaningCrescent = "WNC"

  display Expanded NewMoon        = "New Moon"
  display Expanded WaxingCrescent = "Waxing Crescent"
  display Expanded FirstQuarter   = "First Quarter"
  display Expanded WaxingGibbous  = "Waxing Gibbous"
  display Expanded FullMoon       = "Full Moon"
  display Expanded WaningGibbous  = "Waning Gibbous"
  display Expanded LastQuarter    = "Third Quarter"
  display Expanded WaningCrescent = "Waning Crescent"

instance Display WeatherCondition where
  display _ Clear        = "Clear"
  display _ Cloudy       = "Cloudy"
  display _ PartlyCloudy = "Partly Cloudy"
  display _ MostlyCloudy = "Mostly Cloudy"
  display _ Rain         = "Rain"
  display _ RainPartial  = "Partial Rain"
  display _ Thunderstorm = "Thunderstorm"
  display _ Tornado      = "Tornado"
  display _ Snow         = "Snow"
  display _ Sleet        = "Sleet"
  display _ Fog          = "Fog"
  display _ Mist         = "Mist"
  display _ Haze         = "Haze"
  display _ Smoke        = "Smoke"

instance Display a => Display (Maybe a) where
  display style (Just x) = display style x
  display _      Nothing = ""

instance Display Temperature where
  display Compact  (Kelvin t)     = showRound t <> " K"
  display Expanded (Kelvin t)     = showRound t <> " Kelvin"

  display Compact  (Celsius t)    = showRound t <> "°C"
  display Expanded (Celsius t)    = showRound t <> "° Celsius"

  display Compact  (Fahrenheit t) = showRound t <> "°F"
  display Expanded (Fahrenheit t) = showRound t <> "° Farenheit"

instance Display (Temperature, Temperature) where
  display style (high, low) = display style high <> " - " <> display style low

instance Display Speed where
  display _ = display'
    where
      display' (MilesPerHour s)  = f s "mph"
      display' (MetresPerSecond s) = f s "m/s"
      f x u = S.show (round x :: Integer) <> " " <> u

instance Display RelativeHumidity where
  display _ (RelativeHumidity rH) = S.show rH <> "%"

instance Display UVI where
  display Compact  (UVI u) = S.show (round u :: Integer)
  display Expanded (UVI u) = S.show (round u :: Integer) <> " - " <> severity
    where
      severity
        | 0    <= u && u < 2.5  = "Low"
        | 2.5  <= u && u < 5.5  = "Moderate"
        | 5.5  <= u && u < 7.5  = "High"
        | 7.5  <= u && u < 10.5 = "Very High"
        | 10.5 <= u             = "Extreme"
        | otherwise             = "(Out of Range)"

instance Display CardinalDirection where
  display Compact  NorthWest = "NW"
  display Compact  North     = "N"
  display Compact  NorthEast = "NE"
  display Compact  West      = "W"
  display Compact  East      = "E"
  display Compact  SouthWest = "SW"
  display Compact  South     = "S"
  display Compact  SouthEast = "SE"

  display Expanded NorthWest = "Northwest"
  display Expanded North     = "North"
  display Expanded NorthEast = "Northeast"
  display Expanded West      = "West"
  display Expanded East      = "East"
  display Expanded SouthWest = "Southwest"
  display Expanded South     = "South"
  display Expanded SouthEast = "Southeast"

instance Display WindVelocity where
  display style (WindVelocity dir speed) = display style dir <> display style speed

instance Display Pressure where
  display _ (Pressure p) = S.show p <> " hPa"

instance Symbol StaticIcon where
  symbol Thermometer       = "🌡️"
  symbol DropletWide       = "🌢"
  symbol SunFlat           = "☼"
  symbol TriangleUp        = "▲ "
  symbol TriangleDown      = "▼ "
  symbol TriangleUpSmall   = "⏶ "
  symbol TriangleDownSmall = "⏷ "

instance Symbol CardinalDirection where
  symbol NorthWest = "↖ "
  symbol North     = "↑ "
  symbol NorthEast = "↗ "
  symbol West      = "← "
  symbol East      = "→ "
  symbol SouthWest = "↙ "
  symbol South     = " ↓"
  symbol SouthEast = "↘ "

instance Symbol WindVelocity where
  symbol (WindVelocity dir _) = symbol dir

instance Symbol WeatherCondition where
  symbol Clear        = "☀️ "
  symbol Cloudy       = "☁ "
  symbol PartlyCloudy = "🌤️"
  symbol MostlyCloudy = "🌥️"
  symbol Rain         = "🌧️"
  symbol RainPartial  = "🌦️"
  symbol Thunderstorm = "⛈️"
  symbol Tornado      = "🌪️"
  symbol Snow         = "❄️"
  symbol Sleet        = "🌨️"
  symbol Fog          = "🌫️"
  symbol Mist         = "🌁"
  symbol Haze         = "≋≋"
  symbol Smoke        = "🔥"

instance Symbol MoonPhase where
  symbol NewMoon        = "🌑"
  symbol WaxingCrescent = "🌒"
  symbol FirstQuarter   = "🌓"
  symbol WaxingGibbous  = "🌔"
  symbol FullMoon       = "🌕"
  symbol WaningGibbous  = "🌖"
  symbol LastQuarter    = "🌗"
  symbol WaningCrescent = "🌘"

instance Symbol PressureLevel where
  symbol HighPressure   = "🅗 "
  symbol NormalPressure = "⚫"
  symbol LowPressure    = "🅛 "

instance Symbol a => Symbol (Maybe a) where
  symbol Nothing  = ""
  symbol (Just x) = symbol x

showRound :: Double -> S.Text
showRound x = S.show (round x :: Integer)

fSymbol :: (Symbol a) => a -> S.Text
fSymbol t = padChar $ symbol t

padCenterLeft :: Int -> S.Text -> S.Text
padCenterLeft w content = leftPad <> content <> rightPad
  where
    leftPad       = S.replicate padLength " "
    rightPad      = S.replicate (padLength - leftPadLength) " "
    padLength     = w - S.length content
    leftPadLength = padLength `div` 2

-- Emojis are not consistently displayed in a terminal
-- These manual adjustments are likely to change
padChar :: S.Text -> S.Text
padChar ch = ch <> spaces where
  spaces 
    | ch `elem` [ "❄️ "
      , "☀️ "
      , "⛈️ "
      ] = ""
    | otherwise   = " "

