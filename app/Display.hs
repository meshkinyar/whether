{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExistentialQuantification #-}

module Display where

import Types
import Conversions
import Data.Time.Format
import Data.Time.Clock.POSIX    ( posixSecondsToUTCTime )
import Data.Time.Clock          ( UTCTime )
import Formatting
import qualified Types as D     ( Daily(dt, sunrise, sunset, pressure, weather, d_temp, uvi, d_rain, d_snow, humidity, wind_speed, wind_deg)
                                , DailyTemp(max, min)
                                )
import qualified Types as C     ( Current(humidity, weather) )
import qualified Data.Text as S ( Text, pack, unpack, show, intercalate, replicate, length )

newtype DTStyle = DayStyle DayStyle

data DayStyle = DayAbbr | DateDash

data Element = Border Border
             | forall a. (Component' a) => Component' (Forecast -> [a])

data Border = Top
            | Divider
            | Bottom

data DisplayStyle = Compact | Expanded

data FrameProperties = FrameProperties
  { lineStyle    :: LineStyle
  , dtStyle      :: DTStyle
  , contentStyle :: ContentStyle
  , colWidth     :: Int
  , mode         :: DisplayStyle
  }

data Frame = Frame
  { properties :: FrameProperties
  , order      :: [Element]
  }

data Row = Row S.Text S.Text S.Text

data Cell = C1 [S.Text] | C2 [(S.Text, S.Text)]

data LineStyle = Rounded

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
  display Compact  NewMoon        = "NM"
  display Compact  WaxingCrescent = "WXC"
  display Compact  FirstQuarter   = "FQ"
  display Compact  WaxingGibbous  = "WXG"
  display Compact  FullMoon       = "FM"
  display Compact  WaningGibbous  = "WNG"
  display Compact  LastQuarter    = "LQ"
  display Compact  WaningCrescent = "WNC"

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

instance Symbol a => Symbol (Maybe a) where
  symbol Nothing  = ""
  symbol (Just x) = symbol x

class CellInner a where
  concatWrap :: [a] -> a

instance CellInner S.Text where
  concatWrap t = S.pack $ concat $ wrapLine (map S.unpack t)
    where
    wrapLine []   = []
    wrapLine [x]  = [x]
    wrapLine (x:xs) = x : "│" : wrapLine xs

instance CellInner (S.Text, S.Text) where
  concatWrap tuplelist = (concatWrap f, concatWrap s)
    where
      (f, s) = unzip tuplelist

class Component' a where
  displayB :: FrameProperties -> [a] -> Cell
  displayE :: FrameProperties -> [a] -> Cell

instance Component' UTCTime where
  displayB fp ts = C1 $ map bFmt ts
    where
      bFmt = dateFormat (dtStyle fp) (colWidth fp)
  displayE fp ts = C1 $ map eFmt ts
    where
      eFmt = dateFormat (dtStyle fp) (colWidth fp)

instance Component' (Maybe WeatherCondition) where
  displayB fp wcs = C1 $ map bFmt wcs
    where
      bFmt wc = compactFormat fp "  " $ maybe "  " fSymbol wc
  displayE fp wcs = C1 $ map eFmt wcs
    where
      eFmt wc = expandedFormat fp (glyph $ contentStyle fp) (maybe "  " (display Expanded) wc)
        where
          glyph Symbolic = maybe "  " fSymbol wc
          glyph Textual  = "WF"

instance Component' (Temperature, Temperature) where
  displayB fp lhs = C2 $ map bFmt lhs
    where
      bFmt (l, h) = (f "⏶ " h, f "⏷ " l)
      f ico t = compactFormat fp ico (display Compact t <> " ")
  displayE fp lhs = C1 $ map eFmt lhs
    where
      eFmt (l, h) = expandedFormat fp (glyph $ contentStyle fp) (display Expanded l <> " - " <> display Expanded h)
      glyph Symbolic = fSymbol Thermometer 
      glyph Textual  = "T "

-- instance Component' RelativeHumidity where
--   displayB fp hs = C1 $ map bFmt hs
--   where
--   bFmt rh = CompactFormat fp "  " $ S.show rh
--   displayE fp hs = C1 $ map eFmt hs
--   where
--   eFmt rh t = expandedFormat fp (fSymbol DropletWide) $ (S.show rh <> "% ") <> hiIndicator <> " " <> (S.show $ hiDiff t)
--   where
--   hi  = toHeatIndex t rh
--   hiDiff t' = liftT2 (-) t' t
--   hiIndicator
--     | numT (hiDiff hi) < 0 = glyph TriangleDown "-"
--     | otherwise    = glyph TriangleUp   "+"
--   glyph ico txt = case (contentStyle fp) of
--     Symbolic -> fSymbol ico
--     Textual  -> txt

instance Component' (Maybe WindVelocity) where
  displayB fp ws = C1 $ map bFmt ws
    where
      bFmt w = compactFormat fp " " $ maybe "  " (\(WindVelocity _ s) -> bShow s) w
      bShow (MilesPerHour s)  = f s "mph"
      bShow (MetresPerSecond s) = f s "m/s"
      f x u = S.show (round x :: Integer) <> u
  displayE fp ws = C1 $ map eFmt ws
    where
      eFmt Nothing     = expandedFormat fp "  " "  "
      eFmt (Just (WindVelocity d s)) = expandedFormat fp (glyph d) (display Compact s)
      glyph = case contentStyle fp of
        Symbolic -> fSymbol
        Textual  -> display Compact

-- data Fragment = Static S.Text
--     | Dynamic (Forecast' -> S.Text)
-- 
-- -- Component' field Accessors
-- 
-- conditions' :: Fragment
-- conditions' = Dynamic $ f where
--   isDay df = DF.time df >= DF.sunrise df && DF.time df <= DF.sunset df
--   f (DF' forecast) = case (con, isDay forecast) of
--   (Just Clear, True) -> "✨"
--   (Just c, _)  -> symbol c
--   (Nothing, _)   -> " "
--   where
--   con = DF.condition forecast
-- 
-- temperatureHigh :: Fragment
-- temperatureHigh = Dynamic $ f where
--   f (DF' df) = S.show $ fst $ DF.temperature df  

showRound :: Double -> S.Text
showRound x = S.show (round x :: Integer)

fSymbol :: (Symbol a) => a -> S.Text
fSymbol t = padChar $ symbol t

dateFormat :: (FormatTime a) => DTStyle -> Int -> a -> S.Text
dateFormat s w date = sformat (stext % stext % stext) l c r
  where
  c      = S.pack $ formatTime defaultTimeLocale (fstr s) date
  (l, r)     = (S.replicate s1 " ", S.replicate s2 " ") 
  s1       = padLength `div` 2
  s2       = s1 + padLength `mod` 2
  padLength    = w - S.length c
  fstr (DayStyle DayAbbr)  = " %a "
  fstr (DayStyle DateDash) = "%m-%d"

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

borderRow :: LineStyle -> Border -> Int -> Int -> Row
borderRow st btype w n = Row l m r
  where
    (l, m, r) = case st of 
      Rounded -> case btype of
        Top   -> ("╭", borderMiddle "┬", "╮")
        Divider -> ("├", borderMiddle "┼", "┤")
        Bottom  -> ("╰", borderMiddle "┴", "╯")
    borderMiddle :: S.Text -> S.Text
    borderMiddle x = S.intercalate x $ replicate n line
      where
         line = S.replicate w "─"

-- contentRow :: (a -> S.Text) -> [a] -> Row
-- contentRow formatter li = Row "│" body "│"
--   where
--   body = concatWrap $ map formatter li

unwrapRow :: Row -> S.Text
unwrapRow (Row x y z) = sformat (stext % stext % stext) x y z
  
compactFormat :: FrameProperties -> S.Text -> S.Text -> S.Text
compactFormat fp icoL t = sformat (" " % stext % stext % stext) icoL t padR
  where
  padR = S.replicate lenR " "
  lenR = colWidth fp - S.length t - S.length icoL - 1

expandedFormat :: FrameProperties -> S.Text -> S.Text -> S.Text
expandedFormat fp x y = sformat (" " % stext % " " % stext % stext) x y padR
  where
  padR = S.replicate lenR " "
  lenR = colWidth fp - S.length y - S.length x - 2

getWeatherCondition :: [Weather] -> Maybe WeatherCondition
getWeatherCondition (x:_) = toWeatherCondition $ x ^. id
getWeatherCondition [] = Nothing

newDailyForecast :: UnitSystem -> Daily -> Forecast
newDailyForecast units d = DailyForecast
  { time             = toUTCTime D.dt
  , sunrise          = toUTCTime D.sunrise
  , sunset           = toUTCTime D.sunset
  , weatherCondition = getWeatherCondition $ D.weather d
  , temperatureHigh  = toTemp D.min
  , temperatureLow   = toTemp D.max
  , humidity         = RelativeHumidity $ D.humidity d
  , pressure         = Pressure $ D.pressure d
  , windVelocity     = windF $ D.wind_deg d
  , uvIndex          = UVI $ D.uvi d
  , rainfall         = toPrecip D.d_rain
  , snowfall         = toPrecip D.d_snow
  , moon             = toMoonPhase $ moon_phase d
  }
  where
    toUTCTime fa = posixSecondsToUTCTime $ fa d
    toTemp fa = toTemperature units (fa $ D.d_temp d)
    toPrecip fa = do
      precip <- fa d
      return (toPrecipitation units precip)
    windF direction = do
      dir <- toCardinalDirection direction
      return $ WindVelocity dir (toSpeed units (D.wind_speed d))

getCurrentWeather :: Config -> OneCallRoot -> Forecast
getCurrentWeather config oneCall = CurrentWeather
  { weatherCondition = getWeatherCondition $ C.weather $ current oneCall
  , temperature      = toTemperature (unitSystem config) (c_temp $ current oneCall)
  , humidity         = RelativeHumidity $ C.humidity $ current oneCall
  , moon             = case daily oneCall of
      x:_ -> toMoonPhase $ moon_phase x
      []  -> Nothing
  }

-- Formatters

statusString :: Forecast -> S.Text
statusString CurrentWeather{weatherCondition, temperature, humidity, moon} =
  sformat fStr (fSymbol weatherCondition) (display Compact temperature) (display Compact humidity) (fSymbol moon)
  where
  fStr  = stext % stext % " 🌢  " % stext % "% " % stext
statusString DailyForecast{} = undefined

formatRows :: FrameProperties -> Int -> [Forecast] -> Element -> [Row]
formatRows fp n _ (Border b) = [Row l m r]
  where
    (l, m, r) = case lineStyle fp of 
      Rounded -> case b of
        Top     -> ("╭", borderMiddle "┬", "╮")
        Divider -> ("├", borderMiddle "┼", "┤")
        Bottom  -> ("╰", borderMiddle "┴", "╯")
    borderMiddle :: S.Text -> S.Text
    borderMiddle x = S.intercalate x $ replicate n line
      where
        line = S.replicate (colWidth fp) "─"


makeRows :: Cell -> [Row]
makeRows (C1 ts) = [Row "│" (concatWrap ts) "│"]
makeRows (C2 ts) = [Row "│" (concatWrap f) "│", Row "│" (concatWrap s) "│"]
  where
  (f, s) = unzip ts
