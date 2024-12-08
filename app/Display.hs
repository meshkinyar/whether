{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Display where

import Types
import Conversions
import Data.Time.Format
import Data.Time.Clock.POSIX ( POSIXTime, posixSecondsToUTCTime )
import Formatting
import qualified Types as D (Daily(dt, pressure, weather, d_temp, uvi, d_rain, d_snow, humidity, wind_speed, wind_deg), DailyTemp(max, min))
import qualified Types as C (Current(temp, humidity, weather))
import qualified Types as CW (CurrentWeather(temp, cond, rH, moon))
import qualified Types as DF (DailyForecast(cond))
import qualified Data.Text as T ( Text, pack, unpack, unlines, show, intercalate, replicate, length )

data DayStyle = DayAbbr | DateDash

data BorderType = BorderTop
                | BorderDivider
                | BorderBottom

data Row = Row T.Text T.Text T.Text
data BoxStyle = Rounded

data Frame = Frame
    { style    :: BoxStyle
    , captions :: Row
    , content  :: [Row]
    }

data StaticIcon = TemperatureIcon
                | HumidityIcon
                | UVIIcon
                | TriangleUpIcon
                | TriangleDownIcon

data ContentStyle = Textual | Symbolic

class Symbol a where
    symbol :: a -> T.Text

instance Symbol StaticIcon where
    symbol ico = case ico of
        TemperatureIcon  -> "🌡️"
        HumidityIcon     -> "🌢"
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
        South     -> "↓ "
        SouthEast -> "↘ "

instance Symbol WeatherCondition where
    symbol condition = case condition of
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

fSymbol :: (Symbol a) => a -> T.Text
fSymbol t = padChar $ symbol t

fDate :: DayStyle -> POSIXTime -> T.Text
fDate s date = T.pack $ formatTime defaultTimeLocale (fstr s) $ posixSecondsToUTCTime date
  where
    fstr DayAbbr  = " %a "
    fstr DateDash = "%m-%d"

-- Emojis are not consistently displayed in a terminal
-- These manual adjustments are likely to change
padChar :: T.Text -> T.Text
padChar ch = ch <> (T.replicate p " ") where
    p | ch `elem` [ "☁ "
                  , "✨"
                  , "🌁"
                  ] = 1
      | ch `elem` [ "❄️ "
                  , "☀️ "
                  , "⛈️ "
                  ] = 0
      | otherwise   = 1


borderRow :: BorderType -> Int -> Int -> Row
borderRow bt w n = Row l m r
  where
    (l, m, r) = case bt of
        BorderTop     -> ("╭", borderMiddle "┬", "╮")
        BorderDivider -> ("├", borderMiddle "┼", "┤")
        BorderBottom  -> ("╰", borderMiddle "┴", "╯")
    borderMiddle :: T.Text -> T.Text
    borderMiddle x = T.intercalate x $ replicate n line
      where
        line = T.replicate w "─"

contentRow :: (a -> T.Text) -> [a] -> Row
contentRow formatter li = Row "│" body "│"
  where
    body = concatWrap $ map formatter li
        
miniFormat :: T.Text -> T.Text -> T.Text
miniFormat icoL s = sformat (" " % stext % stext % stext) icoL s padR
  where
    padR = T.replicate lenR " "
    lenR = 8 - T.length s - T.length icoL

compFormat :: T.Text -> T.Text -> T.Text
compFormat = sformat (" " % stext % "  " % stext % " ")

---- Element formatters

miniCond :: Maybe WeatherCondition -> T.Text
miniCond c = miniFormat "  " $ maybe "" fSymbol c

compCond :: ContentStyle -> WeatherCondition -> T.Text
compCond s c = compFormat (glyph s) info
  where
    glyph Symbolic = fSymbol c
    glyph _ = "F "
    info = T.show c


miniTemps :: (Temperature, Temperature) -> (T.Text, T.Text)
miniTemps (l, h) = (f "⏷ " l, f "⏶ " h) 
  where
    f i t = miniFormat i (T.show t <> " ")

compTemps :: (Temperature, Temperature) -> T.Text
compTemps (h, l) = compFormat (fSymbol TemperatureIcon)
                              (T.show l <> " - " <> T.show h)

compHumidity :: Temperature -> Integer -> T.Text
compHumidity (T u t) rh = compFormat 
    (fSymbol hiIndicator)
    (T.show rh <> (symbol hiIndicator) <> " " <> (T.show $ hiDiff hi))
  where
    hiDiff (T _ t') = round1d (t' - t)
    round1d = \x -> (fromIntegral (round (x * 10) :: Integer) :: Double) / 10
    hi = toHeatIndex (T u t) rh
    hiIndicator
        | hiDiff hi < 0 = TriangleDownIcon
        | otherwise     = TriangleUpIcon

getWeatherCondition :: Bool -> [Weather] -> Maybe WeatherCondition
getWeatherCondition y (x:_) = toWeatherCondition y $ weather_id x
getWeatherCondition _ [] = Nothing

getDailyForecast :: Config -> OneCallRoot -> [DailyForecast]
getDailyForecast config oneCall =
    map newDF $ daily oneCall
      where
        newDF d = DailyForecast
          { time  = D.dt d
          , cond  = getWeatherCondition True $ D.weather d
          , temps = (t D.min, t D.max)
          , rH    = D.humidity d
          , hPa   = D.pressure d
          , wind  = windF $ D.wind_deg d
          , uvi   = D.uvi d
          , rain  = precip D.d_rain
          , snow  = precip D.d_snow
          }
          where
            t f  = T (unitSystem config)
                     (f $ D.d_temp d)
            precip f = do
                p <- f d
                return (toPrecipitation config p)
            windF direction = do
                dir <- toCardinalDirection direction
                return $ Wind (unitSystem config)
                              dir
                              (D.wind_speed d)

getCurrentWeather :: Config -> OneCallRoot -> CurrentWeather
getCurrentWeather config oneCall = CurrentWeather
    { cond    = getWeatherCondition (isDayCurrent $ current oneCall)
              $ C.weather $ current oneCall
    , temp    = T (unitSystem config) (C.temp $ current oneCall)
    , rH      = C.humidity $ current oneCall
    , moon    = case (daily $ oneCall) of
                    x:_ -> toMoonPhase $ moon_phase $ x
                    []  -> Nothing
    }

concatWrap :: [T.Text] -> T.Text
concatWrap t = T.pack $ concat $ wrapLine (map T.unpack t)
  where
    wrapLine []     = []
    wrapLine [x]    = [x]
    wrapLine (x:xs) = x : "│" : wrapLine xs

miniDate :: DayStyle -> POSIXTime -> T.Text
miniDate ds t = miniFormat " " (fDate ds t)

-- Formatters

statusString :: CurrentWeather -> T.Text
statusString cw =
    sformat fStr (mb (padE . symbol) CW.cond)
                 (element CW.temp)
                 (element CW.rH)
                 (mb T.show CW.moon)
      where
        fStr      = stext % stext % " 🌢  " % stext % "% " % stext
        element x = T.show $ x cw
        mb y0 y1  = maybe "  " y0 (y1 cw)
        padE z | z == padChar z = z
               | otherwise       = z <> " "

basicForecast :: Int -> [DailyForecast] -> T.Text
basicForecast n df =
    T.unlines $ map interpBox
        [
          border BorderTop
        , dateStr
        , border BorderDivider
        , condition 
        , temperature snd
        , temperature fst
        , border BorderBottom
        ]
    where
      border bt = borderRow bt 9 n 
      days = take n df
      interpBox (Row x y z) = sformat (stext % stext % stext) x y z
      condition = contentRow (miniCond . DF.cond) days
      temperature f = contentRow (f . miniTemps . temps) days
      dateStr = contentRow (miniDate DayAbbr . time) days
