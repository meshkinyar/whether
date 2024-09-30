module Box where

import Types
import Conversions
import qualified Types as Day (Daily(dt, weather, d_temp), DailyTemp(max, min))
import Data.List
import Data.Time.Format
import Data.Time.Clock.POSIX ( POSIXTime, posixSecondsToUTCTime )

data DayStyle = DayAbbr | DateDash

data SimpleForecast = SimpleForecast
    { time :: POSIXTime
    , cond :: WeatherCondition
    , high :: Temperature
    , low  :: Temperature
    }

getSimpleForecast :: Int -> Config -> OneCallRoot -> [SimpleForecast]
getSimpleForecast nDays config oneCall =
    map newSF days
      where
        days    = take nDays $ daily oneCall
        newSF d = SimpleForecast { time = Day.dt d
                                 , cond = condition $ Day.weather d
                                 , high = temp' Day.max
                                 , low  = temp' Day.min
                                 }
          where
            condition (x:_) = toWeatherCondition True $ weather_id x
            condition [] = error "Could not get weather condition for at least one day"
            temp' f   = T (f $ Day.d_temp d) $ units config 

concatWrap :: [String] -> String
concatWrap str =
    concat $ wrapLine str
      where
        wrapLine []     = []
        wrapLine [x]    = [x]
        wrapLine (x:xs) = x : "│" : wrapLine xs

dateStr :: DayStyle -> POSIXTime -> String
dateStr s date = formatTime defaultTimeLocale fstr $ posixSecondsToUTCTime date
  where 
    fstr = case s of
      DayAbbr  -> " %a "
      DateDash -> "%m-%d"


horizontalLine :: Int -> Int -> String -> String
horizontalLine w n x = intercalate x $ take n $ repeat $ line
  where
    line = take w $ repeat '─'
-- to3DayMatrix :: [Daily] -> [SimpleForecast]

miniForecast :: [SimpleForecast] -> String
miniForecast days =
    unlines $
        map concat
            [
              ["╭", hLine "┬"     , "╮"]
            , ["│", threeCharDay  , "│"]
            , ["├", hLine "┼"     , "┤"]
            , ["│", condF         , "│"]
            , ["│", tempF '↑' high, "│"]  
            , ["│", tempF '↓' low , "│"]  
            , ["╰", hLine "┴"     , "╯"]
            ] 
    where 
      hLine        = horizontalLine 9 (length days)
      threeCharDay = concatWrap [ concat ["  ", dateStr DayAbbr (time d), "  "]
                                         | d <- days
                                         ]
      condF = concatWrap
            $ map pad days
        where
          -- Emojis are not consistently displayed in a terminal
          -- These manual adjustments are likely to change
          pad d = concat $ "   " : show (cond d) : [take n $ repeat ' ']
            where 
              n | cond d `elem` [ Cloudy
                                , ClearNight
                                , Haze
                                , Mist
                                , Smoke
                                ] = 4
                | cond d `elem` [ Snow
                                , ClearDay
                                , Thunderstorm
                                ] = 2
                | otherwise       = 3
      tempF symbol f  =
          concatWrap
        $ map showAdjust days
          where
            showAdjust = padR
                       . (\x -> ' ' : symbol : ' ' : x)
                       . show
                       . f
            padR x = x ++ (take (9 - length x) $ repeat ' ')
