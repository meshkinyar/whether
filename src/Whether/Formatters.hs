{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Whether.Formatters where

import Data.Time.Format
import Formatting.Time
import Formatting.Internal
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text as S

import Whether.Weather
import Whether.Display

-- | Base function for formatters that display data from a @(Forecast).
dynamic :: (a -> S.Text) -> (Forecast -> a) -> Format r (Forecast -> r)
dynamic fmt field = later $ T.fromText . fmt . field

-- | Base function for formatters that display output that only depends on the
-- provided formatting function.
static :: S.Text -> Format r r
static fmt = now $ T.fromText fmt

-- | Convenience method for dynamic formatters with a simple formatting function.
dynamicDisplay :: (Display a) => DisplayStyle -> (Forecast -> a) -> Format r (Forecast -> r)
dynamicDisplay style field = later $ T.fromText . display style . field

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
condition :: DisplayStyle -> Format r (Forecast -> r)
condition style = dynamicDisplay style weatherCondition

-- | Data segment representing the default temperature of the given @Forecast@.
-- DailyForecast displays the high.
-- CurrentWeather displays the current temperature.
temp :: Format r (Forecast -> r)
temp = dynamicDisplay Compact t
  where
    t DailyForecast{temperatureHigh} = temperatureHigh
    t CurrentWeather{temperature}    = temperature

-- | Data segment representing the low temperature of the given @Forecast@.
tempL :: Format r (Forecast -> r)
tempL = dynamicDisplay Compact t
  where
    t DailyForecast{temperatureLow} = temperatureLow
    t CurrentWeather{temperature} = temperature

-- | Data segment representing the high temperature of the given @Forecast@.
tempH :: Format r (Forecast -> r)
tempH = dynamicDisplay Compact t
  where
    t DailyForecast{temperatureHigh} = temperatureHigh
    t CurrentWeather{temperature} = temperature

-- | Data segment representing the humidity of the given @Forecast@.
humid :: Format r (Forecast -> r)
humid = dynamicDisplay Compact h
  where
    h DailyForecast{humidity} = humidity
    h CurrentWeather{humidity} = humidity

-- | Data segment representing the UV index of the given @Forecast@.
uvI :: DisplayStyle -> Format r (Forecast -> r)
uvI style = dynamicDisplay style u
  where
    u DailyForecast{uvIndex} = uvIndex
    u CurrentWeather{} = undefined

-- | Data segment representing the wind velocity of the given @Forecast@.
wind :: DisplayStyle -> Format r (Forecast -> r)
wind style = dynamicDisplay style w
  where
    w DailyForecast{windVelocity} = windVelocity
    w CurrentWeather{} = undefined

date :: (FormatTime a) => DTStyle -> Int -> a -> S.Text
date s w dt = padCenterLeft w $ sformat (fstr s) dt
  where
    fstr (DayStyle DayAbbr)  = " " % dayNameShort % " "
    fstr (DayStyle DateDash) = month >% "-" <> dayOfMonth
