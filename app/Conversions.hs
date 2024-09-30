module Conversions where

import Types
import Data.Ix            ( inRange )
import Data.ByteString    ( ByteString )
import Data.Text          ( pack )
import Data.Text.Encoding ( encodeUtf8 )

import qualified Types as C ( Current(dt, sunrise, sunset) )

-- Convert the OWM moon_phase float to a defined MoonPhase type
toMoonPhase :: Double -> MoonPhase
toMoonPhase x | x `elem` [0, 1]          = NewMoon
              | (x > 0   ) && (x < 0.25) = WaxingCrescent
              | x == 0.25                = FirstQuarter
              | (x > 0.25) && (x < 0.5)  = WaxingGibbous
              | x == 0.50                = FullMoon
              | (x > 0.50) && (x < 0.75) = WaningGibbous
              | x == 0.75                = LastQuarter
              | (x > 0.75) && (x < 1.00) = WaningCrescent
              | otherwise                = error "Invalid value for Moon Phase"

-- Convert the OWM weather condition code to a defined type
toWeatherCondition :: Bool -> Integer -> WeatherCondition
toWeatherCondition isDay x |  inRange  (200, 299) x  = Thunderstorm
                           |  inRange  (300, 399) x
                           || inRange  (502, 599) x  = Rain
                           |  inRange  (500, 502) x  = RainPartial
                           |  inRange  (600, 699) x  = Snow
                           |  x == 701               = Mist
                           |  x == 711               = Smoke
                           |  x `elem` [721, 731,
                                        751, 761]    = Haze
                           |  x == 741               = Fog
                           |  x == 781               = Tornado
                           |  x == 800 && isDay      = ClearDay
                           |  x == 800               = ClearNight
                           |  x == 801               = PartlyCloudy
                           |  x `elem` [802, 803]    = MostlyCloudy
                           |  x == 804               = Cloudy
                           |  otherwise              = error "Weather Condition id is out of range"

-- Check whether the time of day is between sunrise and sunset
isDayCurrent :: Current -> Bool
isDayCurrent c = C.dt c >= C.sunrise c && C.dt c <= C.sunset c

-- Convert between TemperatureUnit and OneCall unit string
oneCallUnits :: TemperatureUnit -> String
oneCallUnits u = case u of
   Kelvin    -> "standard"
   Celsius   -> "metric"
   Farenheit -> "imperial"


-- Format input values for use in a OneCall API call
formatCoord :: Double -> ByteString
formatCoord x = encodeUtf8 $ pack $ show x

formatUnits :: TemperatureUnit -> ByteString
formatUnits x = encodeUtf8 $ pack $ oneCallUnits x
