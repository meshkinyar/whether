{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}

module Whether.Display where

import GHC.Generics
import Data.Int                 ( Int64 )
import Data.Time.Format
import Data.Time.Clock.POSIX    ( posixSecondsToUTCTime )
import Data.Time.Clock          ( UTCTime )
import Formatting
import Formatting.Time
import qualified Data.Text.Lazy as T ( Text, pack, unpack, show, intercalate, replicate, length )

import Whether.Weather
import Whether.Units 

-- | Represents whether to display weather in a compact or expanded form.
data DisplayMode = Compact | Expanded

-- | Represents a single, static, non-ASCII character.
data StaticIcon = Thermometer
                | DropletWide
                | SunFlat
                | TriangleUp
                | TriangleDown
                | TriangleUpSmall
                | TriangleDownSmall

-- | Represents the style used to display indicators and other glyphs.
-- Textual represents indicators as alphanumeric characters.
-- Symbolic uses emojis and other unicode symbols.
data GlyphStyle = Symbolic | Textual
  deriving (Eq, Read, Show, Generic)

-- | Types that have a defined textual representation in this package.
class Display a where
  display :: DisplayMode -> a -> T.Text

-- | Types that have a defined symbolic representation in this package.
class Symbol a where
  symbol :: a -> T.Text

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
  display Compact Clear        = "Clear"
  display Compact Cloudy       = "Cloudy"
  display Compact PartlyCloudy = "P. Cldy"
  display Compact MostlyCloudy = "M. Cldy"
  display Compact Rain         = "Rain"
  display Compact RainPartial  = "P. Rain"
  display Compact Thunderstorm = "Thunder"
  display Compact Tornado      = "Tornado"
  display Compact Snow         = "Snow"
  display Compact Sleet        = "Sleet"
  display Compact Fog          = "Fog"
  display Compact Mist         = "Mist"
  display Compact Haze         = "Haze"
  display Compact Smoke        = "Smoke"

  display Expanded Clear        = "Clear"
  display Expanded Cloudy       = "Cloudy"
  display Expanded PartlyCloudy = "Partly Cloudy"
  display Expanded MostlyCloudy = "Mostly Cloudy"
  display Expanded Rain         = "Rain"
  display Expanded RainPartial  = "Partial Rain"
  display Expanded Thunderstorm = "Thunderstorm"
  display Expanded Tornado      = "Tornado"
  display Expanded Snow         = "Snow"
  display Expanded Sleet        = "Sleet"
  display Expanded Fog          = "Fog"
  display Expanded Mist         = "Mist"
  display Expanded Haze         = "Haze"
  display Expanded Smoke        = "Smoke"

-- Nothing is represented by an empty string.
instance Display a => Display (Maybe a) where
  display style (Just x) = display style x
  display _      Nothing = ""

-- In practice, the expanded representations will likely not be used.
instance Display Temperature where
  display Compact  (Kelvin t)     = showRound t <> " K"
  display Expanded (Kelvin t)     = showRound t <> " Kelvin"

  display Compact  (Celsius t)    = showRound t <> "°C"
  display Expanded (Celsius t)    = showRound t <> "° Celsius"

  display Compact  (Fahrenheit t) = showRound t <> "°F"
  display Expanded (Fahrenheit t) = showRound t <> "° Farenheit"

instance Display Speed where
  display _ = display'
    where
      display' (MilesPerHour s)  = f s "mph"
      display' (MetresPerSecond s) = f s "m/s"
      f x u = T.show (round x :: Integer) <> " " <> u

instance Display RelativeHumidity where
  display _ (RelativeHumidity rH) = T.show rH <> "%"

instance Display UVI where
  display Compact  (UVI u) = T.show (round u :: Integer)
  display Expanded (UVI u) = T.show (round u :: Integer) <> " - " <> severity
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
  display _ (HectoPascal p) = T.show p <> " hPa"

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
  symbol Clear        = "☀️"
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
  symbol Nothing  = "  "
  symbol (Just x) = symbol x

-- | Shows the rounded representation of a double.
showRound :: Double -> T.Text
showRound x = T.show (round x :: Integer)
