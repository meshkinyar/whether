{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ExistentialQuantification #-}

module Display where

import Types
import Conversions
import Data.Time.Format
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Time.Clock ( UTCTime )
import Formatting
import qualified Types as D (Daily(dt, pressure, weather, d_temp, uvi, d_rain, d_snow, humidity, wind_speed, wind_deg), DailyTemp(max, min))
import qualified Types as C (Current(temp, humidity, weather))
import qualified Types as CW (CurrentWeather(temp, cond, rH, moon))
import qualified Types as DF (DailyForecast(time, condition, temperature, humidity, pressure, wind, uvi, rain, snow))
import qualified Data.Text as S ( Text, pack, unpack, show, intercalate, replicate, length, unlines )

data DTStyle = DayStyle DayStyle

data DayStyle = DayAbbr | DateDash

data Element = Border Border
             | forall a. (Component a) => Component (Forecast -> [a])

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

data StaticIcon = TemperatureIcon
                | HumidityIcon
                | UVIIcon
                | TriangleUpIcon
                | TriangleDownIcon

data ContentStyle = Textual | Symbolic

class Symbol a where
    symbol :: a -> S.Text

instance Symbol StaticIcon where
    symbol ico = case ico of
        TemperatureIcon  -> "🌡️"
        HumidityIcon     -> "🌢 "
        UVIIcon          -> "☼"
        TriangleUpIcon   -> "▲"
        TriangleDownIcon -> "▼"

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
        ClearDay     -> "☀️ "
        ClearNight   -> "✨"
        Cloudy       -> "☁ "
        PartlyCloudy -> "🌤️"
        MostlyCloudy -> "🌥️"
        Rain         -> "🌧️"
        RainPartial  -> "🌦️"
        Thunderstorm -> "⛈️ "
        Tornado      -> "🌪️"
        Snow         -> "❄️ "
        Sleet        -> "🌨️"
        Fog          -> "🌫️"
        Mist         -> "🌁"
        Haze         -> "🌁"
        Smoke        -> "🔥"

instance Symbol MoonPhase where
    symbol phase = case phase of
        NewMoon        -> "🌑"
        WaxingCrescent -> "🌒"
        FirstQuarter   -> "🌓"
        WaxingGibbous  -> "🌔"
        FullMoon       -> "🌕"
        WaningGibbous  -> "🌖"
        LastQuarter    -> "🌗"
        WaningCrescent -> "🌘"

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

class Component a where
    displayB :: FrameProperties -> [a] -> Cell
    displayE :: FrameProperties -> [a] -> Cell

instance Component (UTCTime) where
    displayB fp ts = C1 $ map bFmt ts
      where
        bFmt t = dateFormat (dtStyle fp) (colWidth fp) t
    displayE fp ts = C1 $ map eFmt ts
      where
        eFmt t = dateFormat (dtStyle fp) (colWidth fp) t    

instance Component (Maybe WeatherCondition) where
    displayB fp wcs = C1 $ map bFmt wcs
      where
        bFmt wc = basicFormat fp "  " $ maybe "  " fSymbol wc
    displayE fp wcs = C1 $ map eFmt wcs
      where
        eFmt wc = expandedFormat fp (glyph $ contentStyle fp) (maybe "  " S.show wc)
          where
            glyph Symbolic = maybe "  " fSymbol wc
            glyph Textual  = "WF"

instance Component (Temperature, Temperature) where
    displayB fp lhs = C2 $ map bFmt lhs
      where
        bFmt (l, h) = (f "⏶ " h, f "⏷ " l)
        f ico t = basicFormat fp ico (S.show t <> " ")
    displayE fp lhs = C1 $ map eFmt lhs
      where
        eFmt (l, h) = expandedFormat fp (glyph $ contentStyle fp) (S.show l <> " - " <> S.show h)
        glyph Symbolic = fSymbol TemperatureIcon
        glyph Textual  = "T "

instance Component Humidity where
    displayB fp hs = C1 $ map bFmt hs
      where
        bFmt (Humidity rh _) = basicFormat fp "  " $ S.show rh
    displayE fp hs = C1 $ map eFmt hs
      where
        eFmt (Humidity rh t) = expandedFormat fp (fSymbol HumidityIcon) $ (S.show rh <> "% ") <> hiIndicator <> " " <> (S.show $ hiDiff t)
          where
            hi        = toHeatIndex t rh
            hiDiff t' = liftT2 (-) t' t
            hiIndicator
                | numT (hiDiff hi) < 0 = glyph TriangleDownIcon "-"
                | otherwise            = glyph TriangleUpIcon   "+"
            glyph ico txt = case (contentStyle fp) of
                Symbolic -> fSymbol ico
                Textual  -> txt

instance Component (Maybe Wind) where
    displayB fp ws = C1 $ map bFmt ws
      where
        bFmt w = basicFormat fp " " $ maybe "  " (\(Wind _ s) -> bShow s) w
        bShow (MilesPerHour s)    = f s "mph"
        bShow (MetresPerSecond s) = f s "m/s"
        f x u = S.show (round x :: Integer) <> u
    displayE fp ws = C1 $ map eFmt ws
      where
        eFmt Nothing           = expandedFormat fp "  " "  "
        eFmt (Just (Wind d s)) = expandedFormat fp (glyph d) (S.show s)
        glyph = case (contentStyle fp) of
            Symbolic -> fSymbol
            Textual  -> S.show

-- Component field Accessors

times :: Forecast -> [UTCTime]
times (DF f) = map DF.time f

conditions :: Forecast -> [Maybe WeatherCondition]
conditions (DF f) = map DF.condition f

temperatures :: Forecast -> [(Temperature, Temperature)]
temperatures (DF f) = map DF.temperature f

humidities :: Forecast -> [Humidity]
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
    c                        = S.pack $ formatTime defaultTimeLocale (fstr s) date
    (l, r)                   = (S.replicate s1 " ", S.replicate s2 " ") 
    s1                       = padLength `div` 2
    s2                       = s1 + padLength `mod` 2
    padLength                = w - S.length c
    fstr (DayStyle DayAbbr)  = " %a "
    fstr (DayStyle DateDash) = "%m-%d"

-- Emojis are not consistently displayed in a terminal
-- These manual adjustments are likely to change
padChar :: S.Text -> S.Text
padChar ch = ch <> (S.replicate p " ") where
    p | ch `elem` [ "☁ "
                  , "✨"
                  , "🌁"
                  ] = 1
      | ch `elem` [ "❄️ "
                  , "☀️ "
                  , "⛈️ "
                  ] = 0
      | otherwise   = 1

borderRow :: LineStyle -> Border -> Int -> Int -> Row
borderRow st btype w n = Row l m r
  where
    (l, m, r) = case st of 
        Rounded -> case btype of
            Top     -> ("╭", borderMiddle "┬", "╮")
            Divider -> ("├", borderMiddle "┼", "┤")
            Bottom  -> ("╰", borderMiddle "┴", "╯")
    borderMiddle :: S.Text -> S.Text
    borderMiddle x = S.intercalate x $ replicate n line
      where
        line = S.replicate w "─"

-- contentRow :: (a -> S.Text) -> [a] -> Row
-- contentRow formatter li = Row "│" body "│"
--   where
--     body = concatWrap $ map formatter li

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

getWeatherCondition :: Bool -> [Weather] -> Maybe WeatherCondition
getWeatherCondition y (x:_) = toWeatherCondition y $ weather_id x
getWeatherCondition _ [] = Nothing

getDailyForecast :: Config -> OneCallRoot -> Forecast
getDailyForecast config oneCall = DF $
    map newDF $ daily oneCall
      where
        newDF d = DailyForecast
          { time         = posixSecondsToUTCTime $ D.dt d
          , condition    = getWeatherCondition True $ D.weather d
          , temperature  = (t D.min, t D.max)
          , humidity     = Humidity (D.humidity d) avgT
          , pressure     = D.pressure d
          , wind         = windF $ D.wind_deg d
          , uvi          = D.uvi d
          , rain         = precip D.d_rain
          , snow         = precip D.d_snow
          }
          where
            u = unitSystem config
            t f  = toTemperature u (f $ D.d_temp d)
            avgT = liftT2 (\x y -> (x + y) / 2) (t D.min) (t D.max)
            precip f = do
                p <- f d
                return (toPrecipitation u p)
            windF direction = do
                dir <- toCardinalDirection direction
                return $ Wind dir (toSpeed u (D.wind_speed d))

getCurrentWeather :: Config -> OneCallRoot -> CurrentWeather
getCurrentWeather config oneCall = CurrentWeather
    { cond    = getWeatherCondition (isDayCurrent $ current oneCall)
              $ C.weather $ current oneCall
    , temp    = toTemperature (unitSystem config) (C.temp $ current oneCall)
    , rH      = C.humidity $ current oneCall
    , moon    = case (daily $ oneCall) of
                    x:_ -> toMoonPhase $ moon_phase $ x
                    []  -> Nothing
    }

-- Formatters

statusString :: CurrentWeather -> S.Text
statusString cw =
    sformat fStr (maybe "  " (padE . symbol) $ CW.cond cw)
                 (element CW.temp)
                 (element CW.rH)
                 (maybe "  " symbol $ CW.moon cw)
      where
        fStr      = stext % stext % " 🌢  " % stext % "% " % stext
        element x = S.show $ x cw
        padE z | z == padChar z = z
               | otherwise       = z <> " "

basicFrame :: Frame
basicFrame = Frame
    {
      properties = FrameProperties 
          {
            lineStyle    = Rounded
          , dtStyle      = DayStyle DayAbbr
          , contentStyle = Symbolic
          , colWidth     = 9
          , mode         = Basic
          }
    , order      =
          [
            Border Top
          , Component times
          , Border Divider
          , Component conditions
          , Component temperatures
          , Component winds
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
    , order      =
          [
            Border Top
          , Component times 
          , Border Divider
          , Component conditions
          , Component temperatures
          , Component winds
          , Component humidities
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
formatRows fp n (DF forecast) (Component c) = makeRows $ cells (DF $ take n forecast)
  where
    cells f = case (mode fp) of
        Basic    -> displayB fp (c f)
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
