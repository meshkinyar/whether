{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Whether.Formatters where

import Data.Time.Format
import Formatting
import Formatting.Time
import Formatting.Internal
import qualified Data.Text.Lazy as T         ( Text )
import qualified Data.Text.Lazy.Builder as T ( singleton, fromLazyText )

import Whether.Weather
import Whether.Display
import Whether.Units

-- | Base function for formatters that display data from a @(Forecast).
dynamic :: (a -> T.Text) -> (Forecast -> a) -> Format r (Forecast -> r)
dynamic fmt field = later $ T.fromLazyText . fmt . field

-- | Base function for formatters that display output that only depends on the
-- provided formatting function.
static :: T.Text -> Format r r
static fmt = now $ T.fromLazyText fmt

-- | Convenience method for dynamic formatters with a simple formatting function.
dynamicDisplay :: (Display a) => DisplayMode -> (Forecast -> a) -> Format r (Forecast -> r)
dynamicDisplay mode field = later $ T.fromLazyText . display mode . field

-- | Applies the left and right formatters to the same data argument, appending
-- their results with a space. This is just @(<%+>) from Formatting, but with
-- a fixity declaration with precedence so that we can mix it with @(<>)@ as
-- well as other operators defined in this module.
(<+>) :: Format r (a -> r) -> Format r (a -> r)  -> Format r (a -> r)
(<+>) = (<%+>)
infixr 6 <+>

-- | Combine a dynamic and static formatter, outputting a dynamic formatter.
-- Appends the static builder on the right to the dynamic builder on the left.
(>%) :: Format r (a -> r) -> Format r r -> Format r (a -> r)
m >% n =
  Format (\k ->
    runFormat m (\b1 -> runFormat n (\b2 -> k (b1 <> b2)))) 
infixr 7 >%

-- | Like @(>%)@, but adds a space between the two builders.
(>+%) :: Format r (a -> r) -> Format r r -> Format r (a -> r)
m >+% n =
  Format (\k ->
    runFormat m (\b1 -> runFormat n (\b2 -> k (b1 <> T.singleton ' ' <> b2)))) 
infixr 7 >+%

-- | Combine a static and dynamic formatter, outputting a dynamic formatter.
-- Prepends the static builder on the left to the dynamic builder on the right.
(%>) :: Format r r -> Format r (a -> r) -> Format r (a -> r)
m %> n =
  Format (\k ->
    runFormat n (\b1 -> runFormat m (\b2 -> k (b2 <> b1))))
infixr 7 %>

-- | Like @(%>)@, but adds a space between the two builders.
(%+>) :: Format r r -> Format r (a -> r) -> Format r (a -> r)
m %+> n =
  Format (\k ->
    runFormat n (\b1 -> runFormat m (\b2 -> k (b2 <> T.singleton ' ' <> b1))))
infixr 7 %+>

-- | Dynamic indicator for @WeatherCondition@.
-- Displays "WC" if Textual, and an emoji representing the weather if Symbolic.
wc :: ContentStyle -> Format r (Forecast -> r)
wc Textual  = dynamic (const "WC") weatherCondition
wc Symbolic = dynamic fSymbol weatherCondition

-- | Static indicator for @UVI@.
-- Displays "UV" if Textual, and "☼" if symbolic.
uv :: ContentStyle -> Format r r
uv Textual  = static "UV"
uv Symbolic = static $ fSymbol SunFlat

-- | Dynamic indicator for @Wind@'s direction component.
-- Displays a cardinal direction code (e.g. "NW") if Textual, and an arrow
-- pointing in a cardinal direction if Symbolic (e.g. "↖ ").
wd :: ContentStyle -> Format r (Forecast -> r)
wd Textual  = dynamicDisplay Compact windVelocity
wd Symbolic = dynamic fSymbol windVelocity

-- | Static indicator for Humidity.
-- Displays "rH" if Textual, and "🌢 " if Symbolic.
rH :: ContentStyle -> Format r r
rH Textual  = static "rH"
rH Symbolic = static $ fSymbol DropletWide

-- | Static indicator for Temperature.
-- Displays "T " if Textual, and "🌡️ " if Symbolic.
te :: ContentStyle -> Format r r
te Textual  = static "T "
te Symbolic = static $ fSymbol Thermometer

-- | Dynamic indicator for moon phase.
-- Displays "MP" if Textual, and an emoji representing
-- the moon phase if Symbolic.
mp :: ContentStyle -> Format r (Forecast -> r)
mp Textual  = dynamic (const "MP") moon
mp Symbolic = dynamic fSymbol moon

-- | Data segment representing the @weatherCondition@ field for
-- the given @Forecast@.
condition :: DisplayMode -> Format r (Forecast -> r)
condition mode = dynamicDisplay mode weatherCondition

-- | Data segment representing the default temperature of the given @Forecast@.
-- CurrentWeather displays the current temperature.
-- DailyForecast displays the high.
temp :: Format r (Forecast -> r)
temp = dynamicDisplay Compact t
  where
    t CurrentWeather{temperature}    = temperature
    t DailyForecast{temperatureHigh} = temperatureHigh

-- | Data segment representing the low temperature of the given @Forecast@.
tempL :: Format r (Forecast -> r)
tempL = dynamicDisplay Compact t
  where
    t CurrentWeather{temperature}   = temperature
    t DailyForecast{temperatureLow} = temperatureLow

-- | Data segment representing the high temperature of the given @Forecast@.
tempH :: Format r (Forecast -> r)
tempH = dynamicDisplay Compact t
  where
    t CurrentWeather{temperature} = temperature
    t DailyForecast{temperatureHigh} = temperatureHigh

-- | Data segment representing the humidity of the given @Forecast@.
humid :: Format r (Forecast -> r)
humid = dynamicDisplay Compact h
  where
    h CurrentWeather{humidity} = humidity
    h DailyForecast{humidity} = humidity

-- | Data segment representing the UV index of the given @Forecast@.
uvi :: DisplayMode -> Format r (Forecast -> r)
uvi mode = dynamicDisplay mode u
  where
    u CurrentWeather{} = undefined
    u DailyForecast{uvIndex} = uvIndex

-- | Data segment representing the wind velocity of the given @Forecast@.
wind :: DisplayMode -> Format r (Forecast -> r)
wind mode = dynamicDisplay mode w
  where
    w CurrentWeather{windVelocity} = windVelocity
    w DailyForecast{windVelocity} = windVelocity

-- | Data segment showing a formatted datetime of the provided forecast.
datetime :: DTStyle -> Format r (Forecast -> r)
datetime styles = later dt
  where
    dt CurrentWeather{time}                     = bformat (dtDay (dayStyle styles)) time
    dt DailyForecast{time}                      = bformat (dtCurrent (currentStyle styles)) time
    dtDay DayAbbr                               = dayNameShort
    dtDay MonthDay                              = month >% "-" <> dayOfMonth
    dtDay DayMonth                              = dayOfMonth % "-" <> month
    dtCurrent (HourMinute TwelveHour)           = hour12 >% ":" <> minute <> dayHalf
    dtCurrent (HourMinute TwentyFourHour)       = hm
    dtCurrent (DayNameHourMinute notation)      = dayNameShort <> dtCurrent (HourMinute notation)
    dtCurrent (MonthDayHourMinute notation)     = dayNameShort <> dtCurrent (HourMinute notation)
    dtCurrent (YearMonthDayHourMinute notation) = year <> dtCurrent (MonthDayHourMinute notation)

