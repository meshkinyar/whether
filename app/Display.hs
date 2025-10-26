{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ExistentialQuantification #-}

module Display where

import Types
import Conversions
import Data.Time.Format
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Time.Clock ( UTCTime )
import Formatting
import qualified Types as D (Daily(dt, sunrise, sunset, pressure, weather, d_temp, uvi, d_rain, d_snow, humidity, wind_speed, wind_deg), DailyTemp(max, min))
import qualified Types as C (Current(temp, humidity, weather))
import qualified Types as CW (CurrentWeather(temperature, condition, humidity, moon))
import qualified Types as DF (DailyForecast(time, condition, temperature, humidity, pressure, wind, uvi, rain, snow))
import qualified Data.Text as S ( Text, pack, unpack, show, intercalate, replicate, length, unlines )

data DTStyle = DayStyle DayStyle

data DayStyle = DayAbbr | DateDash

data Element = Border Border
             | forall a. (Component' a) => Component' (Forecast -> [a])

data Border = Top
            | Divider
            | Bottom

data DisplayMode = Basic | Expanded

data FrameProperties = FrameProperties
  { lineStyle    :: LineStyle
  , dtStyle      :: DTStyle
  , contentStyle :: ContentStyle
  , colWidth     :: Int
  , mode         :: DisplayMode
  }

data Frame = Frame
  { properties :: FrameProperties
  , order      :: [Element]
  }

data Row = Row S.Text S.Text S.Text

data Cell = C1 [S.Text] | C2 [(S.Text, S.Text)]

data LineStyle = Rounded

data Forecast = DF [DailyForecast]
data Forecast' = DF' DailyForecast

data StaticIcon = Thermometer
                | DropletWide
                | SunFlat
                | TriangleUp
                | TriangleDown
                | TriangleUpSmall
                | TriangleDownSmall

data ContentStyle = Textual | Symbolic

class Display a where
  display :: a -> S.Text

class Symbol a where
  symbol :: a -> S.Text

instance Display MoonPhase where
  display phase = case phase of
    NewMoon        -> "New Moon"
    WaxingCrescent -> "Waxing Crescent"
    FirstQuarter   -> "First Quarter"
    WaxingGibbous  -> "Waxing Gibbous"
    FullMoon       -> "Full Moon"
    WaningGibbous  -> "Waning Gibbous"
    LastQuarter    -> "Third Quarter"
    WaningCrescent -> "Waning Crescent"

instance Display WeatherCondition where
  display c = case c of
     Clear        -> "Clear"
     -- ClearNight   -> "Clear"
     Cloudy       -> "Cloudy"
     PartlyCloudy -> "Partly Cloudy"
     MostlyCloudy -> "Mostly Cloudy"
     Rain         -> "Rain"
     RainPartial  -> "Partial Rain"
     Thunderstorm -> "Thunderstorm"
     Tornado      -> "Tornado"
     Snow         -> "Snow"
     Sleet        -> "Sleet"
     Fog          -> "Fog"
     Mist         -> "Mist"
     Haze         -> "Haze"
     Smoke        -> "Smoke"

instance Display a => Display (Maybe a) where
  display c = case c of
    Just x -> display x
    Nothing -> ""

instance Display Temperature where
  display t = display' t
    where
    display' (Kelvin te)     = f te "K"
    display' (Celsius te)    = f te "°C"
    display' (Fahrenheit te) = f te "°F"
    f x u = S.show (round x :: Integer) <> u

instance Display Speed where
  display speed = display' speed
    where
    display' (MilesPerHour s)    = f s "mph"
    display' (MetresPerSecond s) = f s "m/s"
    f x u = S.show (round x :: Integer) <> " " <> u

instance Display RelativeHumidity where
  display rH = S.show rH <> "%"

instance Display UVI where
  display u
    | 0    <= u && u < 2.5  = "Low"
    | 2.5  <= u && u < 5.5  = "Moderate"
    | 5.5  <= u && u < 7.5  = "High"
    | 7.5  <= u && u < 10.5 = "Very High"
    | 10.5 <= u             = "Extreme"
    | otherwise             = "(Out of Range)"

instance Symbol StaticIcon where
  symbol ico = case ico of
    Thermometer       -> "🌡️"
    DropletWide       -> "🌢 "
    SunFlat           -> "☼"
    TriangleUp        -> "▲"
    TriangleDown      -> "▼"
    TriangleUpSmall   -> "⏶"
    TriangleDownSmall -> "⏷"

instance Symbol CardinalDirection where
  symbol dir = case dir of
    NorthWest -> "↖ "
    North     -> "↑ "
    NorthEast -> "↗ "
    West      -> "← "
    East      -> "→ "
    SouthWest -> "↙ "
    South     -> " ↓"
    SouthEast -> "↘ "

instance Symbol WeatherCondition where
  symbol c = case c of
    Clear    -> "☀️ "
  --  ClearNight   -> "✨"
    Cloudy     -> "☁ "
    PartlyCloudy -> "🌤️"
    MostlyCloudy -> "🌥️"
    Rain     -> "🌧️"
    RainPartial  -> "🌦️"
    Thunderstorm -> "⛈️ "
    Tornado    -> "🌪️"
    Snow     -> "❄️ "
    Sleet    -> "🌨️"
    Fog      -> "🌫️"
    Mist     -> "🌁"
    Haze     -> "🌁"
    Smoke    -> "🔥"

instance Symbol MoonPhase where
  symbol phase = case phase of
    NewMoon    -> "🌑"
    WaxingCrescent -> "🌒"
    FirstQuarter   -> "🌓"
    WaxingGibbous  -> "🌔"
    FullMoon     -> "🌕"
    WaningGibbous  -> "🌖"
    LastQuarter  -> "🌗"
    WaningCrescent -> "🌘"

instance Symbol a => Symbol (Maybe a) where
  symbol Nothing  = ""
  symbol (Just x) = symbol x

class CellInner a where
  concatWrap :: [a] -> a

instance CellInner S.Text where
  concatWrap t = S.pack $ concat $ wrapLine (map S.unpack t)
    where
    wrapLine []     = []
    wrapLine [x]    = [x]
    wrapLine (x:xs) = x : "│" : wrapLine xs

instance CellInner (S.Text, S.Text) where
  concatWrap tuplelist = (concatWrap f, concatWrap s)
    where
    (f, s) = unzip tuplelist

class Component' a where
  displayB :: FrameProperties -> [a] -> Cell
  displayE :: FrameProperties -> [a] -> Cell

instance Component' (UTCTime) where
  displayB fp ts = C1 $ map bFmt ts
    where
    bFmt t = dateFormat (dtStyle fp) (colWidth fp) t
  displayE fp ts = C1 $ map eFmt ts
    where
    eFmt t = dateFormat (dtStyle fp) (colWidth fp) t  

instance Component' (Maybe WeatherCondition) where
  displayB fp wcs = C1 $ map bFmt wcs
    where
    bFmt wc = basicFormat fp "  " $ maybe "  " fSymbol wc
  displayE fp wcs = C1 $ map eFmt wcs
    where
    eFmt wc = expandedFormat fp (glyph $ contentStyle fp) (maybe "  " S.show wc)
      where
      glyph Symbolic = maybe "  " fSymbol wc
      glyph Textual  = "WF"

instance Component' (Temperature, Temperature) where
  displayB fp lhs = C2 $ map bFmt lhs
    where
    bFmt (l, h) = (f "⏶ " h, f "⏷ " l)
    f ico t = basicFormat fp ico (S.show t <> " ")
  displayE fp lhs = C1 $ map eFmt lhs
    where
    eFmt (l, h) = expandedFormat fp (glyph $ contentStyle fp) (S.show l <> " - " <> S.show h)
    glyph Symbolic = fSymbol Thermometer 
    glyph Textual  = "T "

-- instance Component' RelativeHumidity where
--   displayB fp hs = C1 $ map bFmt hs
--     where
--     bFmt rh = basicFormat fp "  " $ S.show rh
--   displayE fp hs = C1 $ map eFmt hs
--     where
--     eFmt rh t = expandedFormat fp (fSymbol DropletWide) $ (S.show rh <> "% ") <> hiIndicator <> " " <> (S.show $ hiDiff t)
--       where
--       hi    = toHeatIndex t rh
--       hiDiff t' = liftT2 (-) t' t
--       hiIndicator
--         | numT (hiDiff hi) < 0 = glyph TriangleDown "-"
--         | otherwise      = glyph TriangleUp   "+"
--       glyph ico txt = case (contentStyle fp) of
--         Symbolic -> fSymbol ico
--         Textual  -> txt

instance Component' (Maybe Wind) where
  displayB fp ws = C1 $ map bFmt ws
    where
    bFmt w = basicFormat fp " " $ maybe "  " (\(Wind _ s) -> bShow s) w
    bShow (MilesPerHour s)  = f s "mph"
    bShow (MetresPerSecond s) = f s "m/s"
    f x u = S.show (round x :: Integer) <> u
  displayE fp ws = C1 $ map eFmt ws
    where
    eFmt Nothing       = expandedFormat fp "  " "  "
    eFmt (Just (Wind d s)) = expandedFormat fp (glyph d) (display s)
    glyph = case (contentStyle fp) of
      Symbolic -> fSymbol
      Textual  -> S.show

-- data Fragment = Static S.Text
--         | Dynamic (Forecast' -> S.Text)
-- 
-- -- Component' field Accessors
-- 
-- conditions' :: Fragment
-- conditions' = Dynamic $ f where
--   isDay df = DF.time df >= DF.sunrise df && DF.time df <= DF.sunset df
--   f (DF' forecast) = case (con, isDay forecast) of
--     (Just Clear, True) -> "✨"
--     (Just c, _)    -> symbol c
--     (Nothing, _)     -> " "
--     where
--       con = DF.condition forecast
-- 
-- temperatureHigh :: Fragment
-- temperatureHigh = Dynamic $ f where
--   f (DF' df) = S.show $ fst $ DF.temperature df  

times :: Forecast -> [UTCTime]
times (DF f) = map DF.time f

conditions :: Forecast -> [Maybe WeatherCondition]
conditions (DF f) = map DF.condition f

temperatures :: Forecast -> [(Temperature, Temperature)]
temperatures (DF f) = map DF.temperature f

humidities :: Forecast -> [RelativeHumidity]
humidities (DF f) = map DF.humidity f

pressures :: Forecast -> [Pressure]
pressures (DF f) = map DF.pressure f

winds :: Forecast -> [Maybe Wind]
winds (DF f) = map DF.wind f

uvis :: Forecast -> [UVI]
uvis (DF f) = map DF.uvi f

rains :: Forecast -> [Maybe Precipitation]
rains (DF f) = map DF.rain f

snows :: Forecast -> [Maybe Precipitation]
snows (DF f) = map DF.snow f

fSymbol :: (Symbol a) => a -> S.Text
fSymbol t = padChar $ symbol t

dateFormat :: (FormatTime a) => DTStyle -> Int -> a -> S.Text
dateFormat s w date = sformat (stext % stext % stext) l c r
  where
  c            = S.pack $ formatTime defaultTimeLocale (fstr s) date
  (l, r)           = (S.replicate s1 " ", S.replicate s2 " ") 
  s1             = padLength `div` 2
  s2             = s1 + padLength `mod` 2
  padLength        = w - S.length c
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
    
basicFormat :: FrameProperties -> S.Text -> S.Text -> S.Text
basicFormat fp icoL t = sformat (" " % stext % stext % stext) icoL t padR
  where
  padR = S.replicate lenR " "
  lenR = (colWidth fp) - S.length t - S.length icoL - 1

expandedFormat :: FrameProperties -> S.Text -> S.Text -> S.Text
expandedFormat fp x y = sformat (" " % stext % " " % stext % stext) x y padR
  where
  padR = S.replicate lenR " "
  lenR = (colWidth fp) - S.length y - S.length x - 2

getWeatherCondition :: [Weather] -> Maybe WeatherCondition
getWeatherCondition (x:_) = toWeatherCondition $ weather_id x
getWeatherCondition [] = Nothing

getDailyForecast :: Config -> OneCallRoot -> Forecast
getDailyForecast config oneCall = DF $
  map newDF $ daily oneCall
    where
    newDF d = DailyForecast
      { time     = posixSecondsToUTCTime $ D.dt d
      , sunrise    = posixSecondsToUTCTime $ D.sunrise d
      , sunset     = posixSecondsToUTCTime $ D.sunset d
      , condition  = getWeatherCondition $ D.weather d
      , temperature  = (t D.min, t D.max)
      , humidity   = D.humidity d
      , pressure   = D.pressure d
      , wind     = windF $ D.wind_deg d
      , uvi      = D.uvi d
      , rain     = precip D.d_rain
      , snow     = precip D.d_snow
      }
      where
      u = unitSystem config
      t f  = toTemperature u (f $ D.d_temp d)
      precip f = do
        p <- f d
        return (toPrecipitation u p)
      windF direction = do
        dir <- toCardinalDirection direction
        return $ Wind dir (toSpeed u (D.wind_speed d))

getCurrentWeather :: Config -> OneCallRoot -> CurrentWeather
getCurrentWeather config oneCall = CurrentWeather
  { condition   = getWeatherCondition $ C.weather $ current oneCall
  , temperature = toTemperature (unitSystem config) (C.temp $ current oneCall)
  , humidity  = C.humidity $ current oneCall
  , moon    = case (daily $ oneCall) of
          x:_ -> toMoonPhase $ moon_phase $ x
          []  -> Nothing
  }

-- Formatters

statusString :: CurrentWeather -> S.Text
statusString cw =
  sformat fStr (maybe "  " (padE . symbol) $ CW.condition cw)
         (element CW.temperature)
         (element CW.humidity)
         (maybe "  " symbol $ CW.moon cw)
    where
    fStr    = stext % stext % " 🌢  " % stext % "% " % stext
    element x = S.show $ x cw
    padE z | z == padChar z = z
         | otherwise     = z <> " "

basicFrame :: Frame
basicFrame = Frame
  {
    properties = FrameProperties 
      {
      lineStyle  = Rounded
      , dtStyle    = DayStyle DayAbbr
      , contentStyle = Symbolic
      , colWidth   = 9
      , mode     = Basic
      }
  , order    =
      [
      Border Top
      , Component' times
      , Border Divider
      , Component' conditions
      , Component' temperatures
      , Component' winds
      , Border Bottom
      ]
  }

expandedFrame :: Frame
expandedFrame = Frame
  {
    properties = FrameProperties
      {
        lineStyle    = Rounded
      , dtStyle      = DayStyle DayAbbr
      , contentStyle = Symbolic
      , colWidth     = 19
      , mode         = Expanded
      }
  , order =
      [
        Border Top
      , Component' times 
      , Border Divider
      , Component' conditions
      , Component' temperatures
      , Component' winds
      , Border Bottom
      ]
  }

formatRows :: FrameProperties -> Int -> Forecast -> Element -> [Row]
formatRows fp n _ (Border b) = [Row l m r]
  where
  (l, m, r) = case (lineStyle fp) of 
    Rounded -> case b of
      Top     -> ("╭", borderMiddle "┬", "╮")
      Divider -> ("├", borderMiddle "┼", "┤")
      Bottom  -> ("╰", borderMiddle "┴", "╯")
  borderMiddle :: S.Text -> S.Text
  borderMiddle x = S.intercalate x $ replicate n line
    where
      line = S.replicate (colWidth fp) "─"
formatRows fp n (DF forecast) (Component' c) = makeRows $ cells (DF $ take n forecast)
  where
    cells f = case (mode fp) of
      Basic  -> displayB fp (c f)
      Expanded -> displayE fp (c f)

makeRows :: Cell -> [Row]
makeRows (C1 ts) = [Row "│" (concatWrap ts) "│"]
makeRows (C2 ts) = [Row "│" (concatWrap f) "│", Row "│" (concatWrap s) "│"]
  where
    (f, s) = unzip ts

makeFrame :: Frame -> Int -> Forecast -> S.Text
makeFrame frame n fs =
  S.unlines
  $ map unwrapRow
  $ concat
  $ map (formatRows (properties frame) n fs) (order frame)
