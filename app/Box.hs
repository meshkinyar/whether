module Box where

import Types
import Data.List
import Data.Time.Format
import Data.Time.Clock.POSIX ( POSIXTime )

data DayStyle = DayAbbr | DateDash

data SimpleForecast = SimpleForecast
    { time :: POSIXTime
    , cond :: WeatherCondition
    , high :: Temperature
    , low  :: Temperature
    }

wrapBorder :: [String] -> [String]
wrapBorder [] = []
wrapBorder ([]:xs) = wrapBorder xs
wrapBorder x@(x'@(x'':_):_) = intersperse boxD x
  where
    boxD = case (x'', last x') of
        ('─','─') -> "┼"
        ('─', _ ) -> "┤"
        ( _, '─') -> "├"
        ( _,  _ ) -> "│"

dateStr :: FormatTime t => DayStyle -> t -> String
dateStr s date = formatTime defaultTimeLocale fstr date
  where 
    fstr = case s of
      DayAbbr  -> " %a "
      DateDash -> "%m-%d"

-- to3DayMatrix :: [Day] -> [SimpleForecast]

miniForecast :: [SimpleForecast] -> String
miniForecast days =
    unlines $
        map concat
            [
              ["╭"   , hLine "┬"   ,      "╮"]
            , ["│"   , threeCharDay,      "│"]
            , ["├"   , hLine "┼"   ,      "┤"]
            , ["│   ", display 1 cond, "   │"]
            , ["│ ↑ ", display 5 high,   " │"]  
            , ["│ ↓ ", display 5 low ,   " │"]  
            , ["╰"   , hLine "┴"   ,      "╯"]
            ] 
    where 
      hLine x         = intercalate x $ take (length days) (repeat "────────")
      threeCharDay    = concat $ wrapBorder [concat ["   ", dateStr DayAbbr (time d), "   "] | d <- days]
      display i z     = concat $ wrapBorder $ map (padR i . show . z) days
        where
          padR j x = x ++ (take (j - length x) $ repeat ' ')

--          f (x:xs) = show " ↑ "


      -- <space><space><space><cond><space><space><
      -- <space><uparr><space><temp><degre><degree>
      -- <space>---------------------------
