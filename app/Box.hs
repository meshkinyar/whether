module Box where

import Types
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

wrapBorder :: [String] -> [String]
wrapBorder [] = []
wrapBorder [x] = [x]
wrapBorder ([]:xs) = wrapBorder xs
wrapBorder (x:[]:xs) = wrapBorder (x:xs)
wrapBorder ( x':y'@(y'':_):x's ) =
    x' : boxD : y' : wrapBorder x's
      where
        boxD = case (y'', last x') of
            ('─','─') -> "┼"
            ('─', _ ) -> "┤"
            ( _, '─') -> "├"
            ( _,  _ ) -> "│"

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
              ["╭"   , hLine "┬"     ,   "╮"]
            , ["│"   , threeCharDay  ,   "│"]
            , ["├"   , hLine "┼"     ,   "┤"]
            , ["│   ", display 2 cond, "  │"]
            , ["│ ↑ ", display 6 high,   "│"]  
            , ["│ ↓ ", display 6 low ,   "│"]  
            , ["╰"   , hLine "┴"     ,   "╯"]
            ] 
    where 
      hLine        = horizontalLine 9 (length days)
      threeCharDay = concat $ wrapBorder [ concat ["  ", dateStr DayAbbr (time d), "  "]
                                         | d <- days
                                         ]
      display w f  = concat $ wrapBorder $ map (padR w . show . f) days
        where
          padR j x = x ++ (take (j - length x) $ repeat ' ')

--          f (x:xs) = show " ↑ "


      -- <space><space><space><cond><space><space><
      -- <space><uparr><space><temp><degre><degree>
      -- <space>---------------------------
