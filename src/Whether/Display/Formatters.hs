{-# LANGUAGE OverloadedStrings #-}

module Whether.Display.Formatters where

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
wc :: GlyphStyle -> Format r (Forecast -> r)
wc Textual  = dynamic (const "WC") weatherCondition
wc Symbolic = dynamic symbol' weatherCondition'
  where
    symbol' (weatherCondition, time, sunrise, sunset)
      | (time < sunrise) || (time > sunset) = "✨"
      | otherwise = symbol Clear
    weatherCondition' df@DailyForecast{} = (weatherCondition df, time df, sunrise df, sunset df)
    weatherCondition' cw@CurrentWeather{} = (weatherCondition cw, time cw, sunrise cw, sunset cw)

-- | Static indicator for @UVI@.
-- Displays "UV" if Textual, and "☼" if symbolic.
uv :: GlyphStyle -> Format r r
uv Textual  = static "UV"
uv Symbolic = static $ symbol SunFlat <> " "

-- | Dynamic indicator for @Wind@'s direction component.
-- Displays a cardinal direction code (e.g. "NW") if Textual, and an arrow
-- pointing in a cardinal direction if Symbolic (e.g. "↖ ").
wd :: GlyphStyle -> Format r (Forecast -> r)
wd Textual  = dynamic display' windVelocity
  where
    display' (Just (WindVelocity cd sp)) = padCD cd
    display' Nothing = "  "
    padCD North = "N "
    padCD West  = "W "
    padCD East  = "E "
    padCD South = "S "
    padCD cd = display Compact cd
wd Symbolic = dynamic symbol windVelocity

-- | Static indicator for Humidity.
-- Displays "rH" if Textual, and "🌢 " if Symbolic.
rH :: GlyphStyle -> Format r r
rH Textual  = static "rH"
rH Symbolic = static $ symbol DropletWide <> " "

-- | Static indicator for Temperature.
-- Displays "T " if Textual, and "🌡️ " if Symbolic.
te :: GlyphStyle -> Format r r
te Textual  = static "T "
te Symbolic = static $ symbol Thermometer

-- | Dynamic indicator for moon phase.
-- Displays "MP" if Textual, and an emoji representing
-- the moon phase if Symbolic.
mp :: GlyphStyle -> Format r (Forecast -> r)
mp Textual  = dynamic (const "MP") moon
mp Symbolic = dynamic symbol moon

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
    t cw@CurrentWeather{} = temperature cw
    t df@DailyForecast{} = temperatureHigh df

-- | Data segment representing the low temperature of the given @Forecast@.
tempL :: Format r (Forecast -> r)
tempL = dynamicDisplay Compact t
  where
    t cw@CurrentWeather{} = temperature cw
    t df@DailyForecast{} = temperatureLow df

-- | Data segment representing the high temperature of the given @Forecast@.
tempH :: Format r (Forecast -> r)
tempH = dynamicDisplay Compact t
  where
    t cw@CurrentWeather{} = temperature cw
    t df@DailyForecast{} = temperatureHigh df


-- | Data segment representing the humidity of the given @Forecast@.
humid :: Format r (Forecast -> r)
humid = dynamicDisplay Compact h
  where
    h cw@CurrentWeather{} = humidity cw
    h df@DailyForecast{} = humidity df

-- | Data segment representing the UV index of the given @Forecast@.
uvi :: DisplayMode -> Format r (Forecast -> r)
uvi mode = dynamicDisplay mode u
  where
    u cw@CurrentWeather{} = uvIndex cw
    u df@DailyForecast{} = uvIndex df

-- | Data segment representing the wind velocity of the given @Forecast@.
wind :: DisplayMode -> Format r (Forecast -> r)
wind mode = dynamic disp w
  where
    w cw@CurrentWeather{} = windVelocity cw
    w df@DailyForecast{} = windVelocity df
    disp (Just (WindVelocity cd s)) = display Expanded s
    disp Nothing = ""

-- | Data segment showing a formatted datetime of the provided forecast.
datetime :: DTStyle -> Format r (Forecast -> r)
datetime styles = later dt
  where
    dt        cw@CurrentWeather{}                      = bformat (dtCurrent (currentStyle styles) $ timeNotation styles) (time cw)
    dt        df@DailyForecast{}                       = bformat (dtDay (dayStyle styles)) (time df)
    dtDay     DayAbbr                               = dayNameShort
    dtDay     MonthDay                              = month >% "-" <> dayOfMonth
    dtDay     DayMonth                              = dayOfMonth % "-" <> month
    dtCurrent HourMinute             TwelveHour     = hour12 >% ":" <> minute <> dayHalf
    dtCurrent HourMinute             TwentyFourHour = hm
    dtCurrent DayNameHourMinute      notation       = dayNameShort <> dtCurrent HourMinute notation
    dtCurrent MonthDayHourMinute     notation       = dayNameShort <> dtCurrent HourMinute notation
    dtCurrent YearMonthDayHourMinute notation       = year <> dtCurrent MonthDayHourMinute notation

