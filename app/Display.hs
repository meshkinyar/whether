{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Display where

import Types
import Conversions
import Data.List ( intercalate )
import Data.Time.Format
import Data.Time.Clock.POSIX ( POSIXTime, posixSecondsToUTCTime )
import Formatting
import qualified Types as D (Daily(dt, weather, d_temp, uvi, d_rain, d_snow, humidity, wind_speed, wind_deg), DailyTemp(max, min))
import qualified Types as C (Current(temp, humidity, weather))
import qualified Types as CW (CurrentWeather(temp, cond, rH, moon))
import qualified Types as DF (DailyForecast(cond))
import qualified Data.Text as T ( Text, pack, unpack, unlines )

data DayStyle = DayAbbr | DateDash

data Row a = Row a a a

-- data SimpleForecast = SimpleForecast
--     { timeS :: POSIXTime
--     , cond  :: WeatherCondition
--     , high  :: Temperature
--     , low   :: Temperature
--     }

getWeatherCondition :: [Weather] -> Maybe WeatherCondition
getWeatherCondition(x:_) = toWeatherCondition True $ weather_id x
getWeatherCondition [] = Nothing 

getDailyForecast :: Config -> OneCallRoot -> [DailyForecast]
getDailyForecast config oneCall =
    map newDF $ daily oneCall
      where
        newDF d = DailyForecast
          { time  = D.dt d
          , cond  = getWeatherCondition $ D.weather d
          , high  = temps D.max
          , low   = temps D.min
          , rH    = D.humidity d
          , wind  = windF $ D.wind_deg d
          , uvi   = D.uvi d
          , rain  = precip D.d_rain
          , snow  = precip D.d_snow
          }
          where
            temps f  = T (unitSystem config)
                         (f $ D.d_temp d)
            precip f = do
                p <- f d
                return (toPrecipitation config p)
            windF direction = do
                dir <- toCardinalDirection direction
                return $ Wind (D.wind_speed d)
                              (unitSystem config)
                              dir

getCurrentWeather :: Config -> OneCallRoot -> CurrentWeather
getCurrentWeather config oneCall = CurrentWeather
    { cond    = getWeatherCondition $ C.weather $ current oneCall
    , temp    = T (unitSystem config) (C.temp $ current oneCall)
    , rH      = C.humidity $ current oneCall
    , moon    = case (daily $ oneCall) of
                    x:_ -> toMoonPhase $ moon_phase $ x
                    []  -> Nothing
    }

concatWrap :: [T.Text] -> T.Text
concatWrap t =
    T.pack $ concat $ wrapLine (map T.unpack t)
      where
        wrapLine []     = []
        wrapLine [x]    = [x]
        wrapLine (x:xs) = x : "│" : wrapLine xs

dateStr :: DayStyle -> POSIXTime -> T.Text
dateStr s date = T.pack $ formatTime defaultTimeLocale fstr
                        $ posixSecondsToUTCTime date
  where
    fstr = case s of
      DayAbbr  -> " %a "
      DateDash -> "%m-%d"


horizontalLine :: Int -> Int -> String -> T.Text
horizontalLine w n x = T.pack $ intercalate x $ take n $ repeat $ line
  where
    line = take w $ repeat '─'

-- Formatters

tmuxStatus :: CurrentWeather -> T.Text
tmuxStatus cw = 
    sformat fStr (g toWeatherSymbol CW.cond) (f CW.temp) (f CW.rH) (g show CW.moon)
      where
        fStr        = stext % string % " 💧 " % string % "% " % string
        f x         = show $ x cw
        g y0 y1     = maybe "  " y0 (y1 cw)


basicForecast :: Int -> [DailyForecast] -> T.Text
basicForecast n df =
    T.unlines $ map interpBox
        [
          Row "╭" (hLine "┬")      "╮"
        , Row "│" threeCharDay     "│"
        , Row "├" (hLine "┼")      "┤"
        , Row "│" condF            "│"
        , Row "│" (tempF '↑' high) "│"  
        , Row "│" (tempF '↓' low)  "│"  
        , Row "╰" (hLine "┴")      "╯"
        ]
    where 
      days = take n df
      interpBox (Row x y z) = sformat (stext % stext % stext) x y z
      hLine        = horizontalLine 9 (length days)
      threeCharDay = concatWrap [ sformat ("  " % stext % "  ")
                                        $ dateStr DayAbbr (time d)
                                | d <- days
                                ]
      condF = concatWrap $ map pad days
      -- Emojis are not consistently displayed in a terminal
      -- These manual adjustments are likely to change
      pad d = sformat ("   " % stext % string)
                      weatherSymbol (take w $ repeat ' ')
        where
          weatherSymbol = case (DF.cond d) of 
              Just x  -> toWeatherSymbol x
              Nothing -> " "
          w | weatherSymbol `elem` [ "☁ "
                                   , "✨"
                                   , "🌁"
                                   ] = 4
            | weatherSymbol `elem` [ "❄️ "
                                   , "☀️ "
                                   , "⛈️ "
                                   ] = 2
            | otherwise              = 3
      tempF symbol f  =
          concatWrap
        $ map showAdjust days
          where
            showAdjust = T.pack 
                       . padR
                       . (\x -> ' ' : symbol : ' ' : x)
                       . show
                       . f
            padR x = x ++ (take (9 - length x) $ repeat ' ')