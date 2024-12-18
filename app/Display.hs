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
import qualified Data.Text as S ( Text, pack, unpack, unlines, show, intercalate, replicate, length )

data DayStyle = DayAbbr | DateDash

data BorderType = BorderTop
                | BorderDivider
                | BorderBottom

data Row = Row S.Text S.Text S.Text
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
    symbol :: a -> S.Text

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

fSymbol :: (Symbol a) => a -> S.Text
fSymbol t = padChar $ symbol t

fDate :: DayStyle -> POSIXTime -> S.Text
fDate s date = S.pack $ formatTime defaultTimeLocale (fstr s) $ posixSecondsToUTCTime date
  where
    fstr DayAbbr  = " %a "
    fstr DateDash = "%m-%d"

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


borderRow :: BorderType -> Int -> Int -> Row
borderRow bt w n = Row l m r
  where
    (l, m, r) = case bt of
        BorderTop     -> ("╭", borderMiddle "┬", "╮")
        BorderDivider -> ("├", borderMiddle "┼", "┤")
        BorderBottom  -> ("╰", borderMiddle "┴", "╯")
    borderMiddle :: S.Text -> S.Text
    borderMiddle x = S.intercalate x $ replicate n line
      where
        line = S.replicate w "─"

contentRow :: (a -> S.Text) -> [a] -> Row
contentRow formatter li = Row "│" body "│"
  where
    body = concatWrap $ map formatter li

expandRow :: Row -> S.Text
expandRow (Row x y z) = sformat (stext % stext % stext) x y z
        
miniFormat :: S.Text -> S.Text -> S.Text
miniFormat icoL s = sformat (" " % stext % stext % stext) icoL s padR
  where
    padR = S.replicate lenR " "
    lenR = 8 - S.length s - S.length icoL

compFormat :: S.Text -> S.Text -> S.Text
compFormat = sformat (" " % stext % "  " % stext % " ")

---- Element formatters

miniCond :: Maybe WeatherCondition -> S.Text
miniCond c = miniFormat "  " $ maybe "" fSymbol c

compCond :: ContentStyle -> WeatherCondition -> S.Text
compCond s c = compFormat (glyph s) info
  where
    glyph Symbolic = fSymbol c
    glyph _ = "F "
    info = S.show c


miniTemps :: (Temperature, Temperature) -> (S.Text, S.Text)
miniTemps (l, h) = (f "⏷ " l, f "⏶ " h) 
  where
    f i t = miniFormat i (S.show t <> " ")

compTemps :: (Temperature, Temperature) -> S.Text
compTemps (h, l) = compFormat (fSymbol TemperatureIcon)
                              (S.show l <> " - " <> S.show h)

compHumidity :: Temperature -> Integer -> S.Text
compHumidity (T u t) rh = compFormat 
    (fSymbol hiIndicator)
    (S.show rh <> (symbol hiIndicator) <> " " <> (S.show $ hiDiff hi))
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

concatWrap :: [S.Text] -> S.Text
concatWrap t = S.pack $ concat $ wrapLine (map S.unpack t)
  where
    wrapLine []     = []
    wrapLine [x]    = [x]
    wrapLine (x:xs) = x : "│" : wrapLine xs

miniDate :: DayStyle -> POSIXTime -> S.Text
miniDate ds t = miniFormat " " (fDate ds t)

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

basicForecast :: Int -> [DailyForecast] -> S.Text
basicForecast n df =
    S.unlines $ map expandRow
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
      condition = contentRow (miniCond . DF.cond) days
      temperature f = contentRow (f . miniTemps . temps) days
      dateStr = contentRow (miniDate DayAbbr . time) days
